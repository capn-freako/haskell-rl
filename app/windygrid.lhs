```include
other/header.md
```

haskell-rl : Windy Gridworld
===

This [literate Haskell](https://wiki.haskell.org/Literate_programming) document provides a solution to Example 6.5 - _Windy Gridworld_.

Original author: [David Banas](mailto:David.Banas@target.com)  
Original date:   June 28, 2018

Copyright &copy; 2018 Target Corp.; all rights reserved World wide.

Contents
---

- [Code](#code)
- [Output](#output)
- [Debug](#debug)

code
---

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
\end{code}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

\begin{code}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
\end{code}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)
- [vector-sized](https://www.stackage.org/package/vector-sized)
- [finite-typelits](https://www.stackage.org/package/finite-typelits)
- [extra](https://www.stackage.org/package/extra)
- [text](https://www.stackage.org/package/text)
- [random](https://www.stackage.org/package/random)
- [random-shuffle](https://www.stackage.org/package/random-shuffle)
- [Chart](https://www.stackage.org/package/Chart)
- [Chart-cairo](https://www.stackage.org/package/Chart-cairo)
- [MemoTrie](https://hackage.haskell.org/package/MemoTrie)

\begin{code}
import qualified Prelude as P
import Prelude (unlines, Show(..), String)
import Protolude  hiding (show, for, first, second)

import Options.Generic

import Control.Arrow
import Control.Monad.Writer
import qualified Data.Vector.Sized   as VS
import Data.Finite
import Data.List                              (sortBy, groupBy)
import Data.MemoTrie
import Data.Text                              (pack)
import Data.Typeable
import Graphics.Rendering.Chart.Easy hiding   (Wrapped, Unwrapped, Empty)
import Graphics.Rendering.Chart.Backend.Cairo
import Statistics.Distribution                (density)
import Statistics.Distribution.Gamma          (gammaDistr)
import Text.Printf

import RL.GPI
\end{code}

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}
{----------------------------------------------------------------------
  Problem specific definitions
----------------------------------------------------------------------}

gGamma   = 1  -- Specified by problem.
gNumRows = 7
gNumCols = 10
gWind    = fromMaybe (P.error "gWind: Sized vector initialization from list failure!")
                     (VS.fromList [0,0,0,1,1,1,2,2,1,0] :: Maybe (VS.Vector 10 Int))

type MyState = (Int, Int)

data MyAction = Up
              | Dn
              | Rt
              | Lt
  deriving (Eq, Ord, Generic)

instance Show MyAction where
  show x = case x of
             Up -> " \\uparrow "
             Dn -> " \\downarrow "
             Rt -> " \\rightarrow "
             Lt -> " \\leftarrow "

instance HasTrie MyAction where
  newtype (MyAction :->: b) = MyActionTrie { unMyActionTrie :: Reg MyAction :->: b } 
  trie      = trieGeneric      MyActionTrie 
  untrie    = untrieGeneric    unMyActionTrie
  enumerate = enumerateGeneric unMyActionTrie

-- | S
allStates :: [MyState]
allStates =
  [ (r, c)
  | r <- [0..(gNumRows - 1)]
  , c <- [0..(gNumCols - 1)]
  ]

-- Just a sized vector alternative to the list above.
--
-- TODO: Figure out how to determine the size from the constants above.
allStatesV :: VS.Vector 70 MyState
allStatesV = fromMaybe (P.error "main.allStatesV: Fatal error converting `allStates`!")
                       $ VS.fromList allStates

-- | A(s)
myActions :: MyState -> [MyAction]
myActions = const [Up, Dn, Rt, Lt]

-- | S'(s, a) - list of next possible states.
myNextStates :: MyState -> MyAction -> [MyState]
myNextStates s@(r, c) act =
  if s `elem` myTermStates
    then [s]
    else [(r', c')]
  where r' = min (gNumRows - 1) (max 0 (r + dr + wr))
        c' = min (gNumCols - 1) (max 0 (c + dc))
        dr = case act of
               Up    ->  1
               Dn  -> -1
               Lt  ->  0
               Rt ->  0
        dc = case act of
               Up    ->  0
               Dn  ->  0
               Lt  -> -1
               Rt ->  1
        wr = gWind `VS.index` (finite $ fromIntegral c)

myTermStates :: [MyState]
myTermStates = [(3,7)]

myStartState :: MyState
myStartState = (3,0)

-- | R(s, a, s')
--
-- Returns a list of pairs, each containing:
-- - a reward value, and
-- - the probability of occurence for that value.
--
-- Note: Previous requirement that reward values be unique eliminated,
--       for coding convenience and runtime performance improvement.
myRewards :: MyState -> MyAction -> MyState -> [(Double, Double)]
myRewards _ _ s' | s' `elem` myTermStates = [( 0, 1)]
                 | otherwise              = [(-1, 1)]

-- | Show a function from `MyState` to `Bool`.
showFofState :: (Show a, Typeable a) => (MyState -> a) -> String
showFofState g = unlines
  ( "\\begin{array}{|" : intersperse '|' (replicate gNumCols 'c') : "|}" :
    ( "\\hline" :
      intersperse "\\hline"
        ( map ((++ " \\\\") . intercalate " & ")
              [ map g'
                [ (r', c)
                | c <- [0..(gNumCols - 1)]
                ]
              | r <- [0..(gNumRows - 1)]
              , let r' = gNumRows - 1 - r
              ]
        )
      ++ ["\\hline"]
      ++ ["\\end{array}"]
    )
  )
  where g' s = if s == myStartState
               then "S"
               else if s `elem` myTermStates
                      then "G"
                      else toString (g s)

toString :: (Show a, Typeable a) => a -> String
toString x = case cast x of
   Just y  -> y
   Nothing -> show x

{----------------------------------------------------------------------
  Command line options defintions.
----------------------------------------------------------------------}

data Opts w = Opts
    { nIter :: w ::: Maybe Int <?>
        "The number of policy improvement iterations"
    , nEval :: w ::: Maybe Int <?>
        "The number of policy evaluation iterations per policy improvement iteration"
    , p     :: w ::: Maybe Int <?>
        "The ratio of stock-out to holding costs"
    , eps   :: w ::: Maybe Double <?>
        "The convergence tolerance for iteration"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)


{----------------------------------------------------------------------
  main()
----------------------------------------------------------------------}

mdFilename = "other/windygrid.md"

boolToDouble :: Bool -> Double
boolToDouble b | b == True = 1
               | otherwise = 0

main :: IO ()
main = do
  -- Process command line options.
  o :: Opts Unwrapped <-
    unwrapRecord "A solution to Example 6.5 - Windy Gridworld."
  let nIters = fromMaybe  2 (nIter o)
      nEvals = fromMaybe  1 (nEval o)
      pVal   = fromMaybe 50 (p     o)
      eps'   = fromMaybe  0.1 (eps o)

  -- Calculate and display optimum policy.
  writeFile mdFilename "\n### Policy optimization\n\n"
  let (fs, counts') = P.unzip $ take (nIters + 1)
                   $ iterate
                       ( optPol
                           rltDef
                             { disc       = gGamma
                             , epsilon    = eps'
                             , maxIter    = nEvals
                             , states     = allStatesV
                             , actions    = myActions
                             , nextStates = myNextStates
                             , rewards    = myRewards
                             , stateVals  = zip myTermStates $ repeat 0
                             }
                       ) (const (Rt, 0), [])
      counts = P.tail counts'
      acts   = map (\f -> VS.map (fst . f) allStatesV) fs
      diffs  = map (VS.map boolToDouble . uncurry (VS.zipWith (/=)))
                  $ zip acts (P.tail acts)
      ((_, g'), cnts) = first (fromMaybe (P.error "main: Major failure!")) $
        -- runWriter $ withinOnM eps'
        runWriter $ withinOnM 0  -- Temporary, to force `nIters` policy improvements.
                              ( \ (dv, _) ->
                                  maxAndNonZero dv
                              ) $ zip diffs (P.tail fs)
      pol    = fst . g'
      val    = snd . g'
      visits = runEpoch 20 pol (((.) . (.)) P.head myNextStates) myTermStates myStartState

  appendFile mdFilename "\n### Final policy\n\n"
  appendFile mdFilename $ pack $ showFofState pol
  appendFile mdFilename "\n### Final value function\n\n"
  appendFile mdFilename $ pack $ showFofState (Pdouble . val)
  appendFile mdFilename "\n### Trajectory of final policy\n\n"
  appendFile mdFilename $ pack $ showFofState $ \s -> if s `elem` visits
                                                        then (" \\cdot " :: String)
                                                        else ""

  -- DEBUGGING
  appendFile mdFilename "\n## debug\n\n"

  -- Value/Action changes vs. Iteration
  toFile def "img/valueDiffs.png" $ do
    layout_title .= "Policy/Value Changes vs. Evaluation Iteration"
    setColors $ map opaque [blue, green, red, yellow, cyan, magenta, brown, gray, purple, black]
    plot ( line "Policy Changes"
                [ [ (x,y)
                  | (x,y) <- zip ( map (* (length $ P.head counts))
                                       [(0::Int)..]
                                 )
                                 cnts
                  ]
                ]
         )
    plot ( line "Value Changes"
                [ [ (x,y)
                  | (x,y) <- zip [(0::Int)..]
                                 (concat counts)
                  ]
                ]
         )
  appendFile mdFilename "\n![](img/valueDiffs.png)\n"

\end{code}

output
---

```include
other/windygrid.md
```

***

<div class="footer">
Powered by [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/) and [pandoc](http://pandoc.org/).
</div>

