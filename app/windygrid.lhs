```include
other/header.md
```

haskell-rl : Stochastically Windy Gridworld
===

This [literate Haskell](https://wiki.haskell.org/Literate_programming) document provides a solution to Ex. 6.10 - _Windy Gridworld w/ King's Moves and Stochastic Wind_.

Original author: [David Banas](mailto:capn.freako@gmail.com)  
Original date:   June 28, 2018

Copyright &copy; 2018 David Banas; all rights reserved World wide.

Contents
---

- [Code](#code)
- [Output](#output)
- => [DP Results](#dp-results)
- => [TD Results](#td-results)
- [Debug](#debug)

code
---

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -cpp #-}
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.MemoTrie
import Data.Text                              (pack)
import Graphics.Rendering.Chart.Easy hiding   (Wrapped, Unwrapped, Empty, Iso)
import Graphics.Rendering.Chart.Backend.Cairo
import Text.Printf

import ConCat.Isomorphism
import ConCat.TArr

import RL.GPI
import RL.MDP
import RL.Util
\end{code}

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}
{----------------------------------------------------------------------
  Problem specific definitions
----------------------------------------------------------------------}

gGamma   = 1  -- Specified by problem.

gWind    = fromMaybe (P.error "gWind: Sized vector initialization from list failure!")
                     (VS.fromList [0,0,0,1,1,1,2,2,1,0] :: Maybe (VS.Vector 10 Int))

type NNumRows = 7
gNumRows = int @NNumRows

type NNumCols = 10
gNumCols = int @NNumCols

type Na = 9

type MyState = (Finite NNumRows, Finite NNumCols)

instance MDP MyState where
  type ActionT MyState = MyAction
  states =
    [ ((finite . fromIntegral) r, (finite . fromIntegral) c)
    | r <- [0..(gNumRows - 1)]
    , c <- [0..(gNumCols - 1)]
    ]
  actions = const [Up, Dn, Rt, Lt, UL, UR, DL, DR, NM]
  jointPMF s@(r,c) act =
    [ ((s', rwd), 0.3333)
    | wr <- let wind = gWind `VS.index` finite (fromIntegral c)
             in if wind /= 0
                  then [wind - 1, wind, wind + 1]
                  else [0, 0, 0]  -- Duplication is to keep probabilities correct.
    , let s' = if s `elem` (map fst termStates)
                 then s
                 else bound ((fromIntegral . getFinite) r + dr + wr, (fromIntegral . getFinite) c + dc)
          dr = case act of
                 Up ->  1
                 Dn -> -1
                 Lt ->  0
                 Rt ->  0
                 UL ->  1
                 UR ->  1
                 DL -> -1
                 DR -> -1
                 NM ->  0
          dc = case act of
                 Up ->  0
                 Dn ->  0
                 Lt -> -1
                 Rt ->  1
                 UL -> -1
                 UR ->  1
                 DL -> -1
                 DR ->  1
                 NM ->  0
          bound (row, col) = ( (finite . fromIntegral) $ min (gNumRows - 1) (max 0 row)
                             , (finite . fromIntegral) $ min (gNumCols - 1) (max 0 col ) )
          rwd = fromIntegral $
            if s' `elem` (map fst termStates)
              then  0
              else -1
    ]
  termStates = [((3,7), 0)]
  initStates = [(3,0)]

data MyAction = Up
              | Dn
              | Rt
              | Lt
              | UL
              | UR
              | DL
              | DR
              | NM  -- no move
  deriving (Eq, Ord, Generic)

instance Show MyAction where
  show x = case x of
             Up -> "\\uparrow "
             Dn -> "\\downarrow "
             Rt -> "\\rightarrow "
             Lt -> "\\leftarrow "
             UL -> "\\nwarrow "
             UR -> "\\nearrow "
             DL -> "\\swarrow "
             DR -> "\\searrow "
             NM -> "\\cdot "

instance HasTrie MyAction where
  newtype (MyAction :->: b) = MyActionTrie { unMyActionTrie :: Reg MyAction :->: b } 
  trie      = trieGeneric      MyActionTrie 
  untrie    = untrieGeneric    unMyActionTrie
  enumerate = enumerateGeneric unMyActionTrie

instance HasFin MyAction where
  type Card MyAction = Na
  iso = Iso actToFin finToAct

actToFin :: MyAction -> Finite Na
actToFin = \case
  Up -> 0
  Dn -> 1
  Rt -> 2
  Lt -> 3
  UL -> 4
  UR -> 5
  DL -> 6
  DR -> 7
  NM -> 8

finToAct :: Finite Na -> MyAction
finToAct = \case
  0 -> Up
  1 -> Dn
  2 -> Rt
  3 -> Lt
  4 -> UL
  5 -> UR
  6 -> DL
  7 -> DR
  _ -> NM

-- | Show a function from `MyState` to `Bool`.
showFofState :: (Show a, Typeable a) => (MyState -> a) -> String
showFofState g = unlines
  ( ("\\begin{array}{|" ++ (intersperse '|' (replicate gNumCols 'c')) ++ "|}") :
    ( "\\hline" :
      intersperse "\\hline"
        ( map ((++ " \\\\") . intercalate " & ")
              [ map g'
                [ ( (finite . fromIntegral) r'
                  , (finite . fromIntegral) c
                  )
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
  where g' s
          | s `elem` initStates           = "S"
          | s `elem` (map fst termStates) = "G"
          | otherwise = toString (g s)

{----------------------------------------------------------------------
  Command line options defintions.
----------------------------------------------------------------------}

data Opts w = Opts
    { nIter :: w ::: Maybe Int <?>
        "The number of TD episodes"
    , nEval :: w ::: Maybe Int <?>
        "The maximum number of state transitions allowed in each episode"
    , nStep :: w ::: Maybe Int <?>
        "The 'n' in n-step TD."
    , eps   :: w ::: Maybe Double <?>
        "Probability of chosing action randomly"
    , dcy   :: w ::: Maybe Double <?>
        "The decay rate for epsilon/alpha"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)

{----------------------------------------------------------------------
  main()
----------------------------------------------------------------------}

mdFilename = "other/windygrid.md"

main :: IO ()
main = do
  -- Process command line options.
  o :: Opts Unwrapped <-
    unwrapRecord "A solution to Example 6.5 - Windy Gridworld."
  let nIters = fromMaybe  10000 (nIter o)
      nEvals = fromMaybe     20 (nEval o)
      nSteps' = fromMaybe     0 (nStep o)
      eps'   = fromMaybe    0.1 (eps   o)
      beta'  = fromMaybe    0   (dcy   o)

  -- Calculate and display optimum policy.
  writeFile mdFilename "\n### DP Results\n\n"

  -- Run DP, to generate reference values.
  let (fs, counts') = P.unzip $ take 20
                   $ iterate
                       ( optPol
                           hypParamsDef
                             { disc       = gGamma
                             , epsilon    = 0.1
                             , maxIter    = 20
                             }
                       ) (const (Rt, 0), [])
      counts = P.tail counts'
      acts   = map (\f -> map (fst . f) states) fs
      diffs  = map (map boolToDouble . uncurry (zipWith (/=)))
                  $ zip acts (P.tail acts)
      g' :: MyState -> (MyAction, Double)
      ((_, g'), cnts') = first (fromMaybe (P.error "main: Major failure!")) $
        runWriter $ withinOnM 0  -- Temporary, to force `nIters` policy improvements.
                              ( \ (dv, _) ->
                                  maxAndNonZero dv
                              ) $ zip diffs (P.tail fs)
      pol    = fst . g'
      val    = snd . g'
      cnts   = cnts'
      visits =
        runEpisode 20 pol (((.) . (.)) (P.head . map fst) nextStates)
          (map fst termStates) $ P.head initStates

  appendFile mdFilename "\n#### Final policy\n\n"
  appendFile mdFilename $ pack $ showFofState pol
  appendFile mdFilename "\n#### Final value function\n\n"
  appendFile mdFilename $ pack $ showFofState (Pdouble . val)
  appendFile mdFilename "\n#### Trajectory of final policy\n\n"
  appendFile mdFilename $ pack $ showFofState $ \s -> if s `elem` visits
                                                        then (" \\cdot " :: String)
                                                        else ""
  -- Value/Action changes vs. Iteration
  appendFile mdFilename "\n#### Policy/Value Function Changes vs. Iteration\n\n"
  toFile def "img/valueDiffs.png" $ do
    layout_title .= "Policy/Value Function Changes vs. Iteration"
    setColors $ map opaque [blue, green, red, yellow, cyan, magenta, brown, gray, purple, black]
    plot ( line "Policy Changes"
                [ [ (x,y)
                  | (x,y) <- zip (map (* (length $ P.head counts)) [(0::Int)..])
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

  -- Run all 3 flavors of TD, comparing results to DP.
  appendFile mdFilename "\n### TD Results\n\n"

  appendFile mdFilename $ pack $ printf "epsilon = %4.2f  \n" eps'
  appendFile mdFilename $ pack $ printf "beta = %8.6f  \n" beta'

  let (erss, termValss) = unzip $ for [0.1, 0.2, 0.5] $ \ alp ->
        let myHypParams =
              hypParamsDef
                { disc       = gGamma
                , epsilon    = eps'
                , alpha      = alp
                , beta       = beta'
                , maxIter    = nEvals
                , tdStepType = Qlearn
                , nSteps     = nSteps'
                }
            ress = for [Sarsa, Qlearn, ExpSarsa] $
                       \ stepT ->
                         doTD myHypParams{tdStepType = stepT} nIters
            vss  = map valFuncs ress
            ers  = map ( map ( \ v -> (/ dpNorm) . mean $
                                        [ sqr (v s - val s)
                                        | s <- states
                                        ]
                             )
                       ) vss
            termVals :: [[Double]]
            termVals = map (map (maximum . map maximum . termQs) . concat . debugs) ress
         in (ers, termVals)
      dpNorm = mean [ sqr (val s)
                    | s <- states
                    ]

  -- Value function error vs. Iteration
  appendFile mdFilename "\n#### Mean Square Value Function Error vs. DP\n\n"
  toFile def "img/vFuncErr.png" $ do
    layout_title .= "Mean Square Value Function Error vs. Iteration"
    forM_ (zip (P.init erss) [PointShapeCircle, PointShapePlus]) $ \ (ers, ptShape) -> do
      setColors $ map opaque [blue, green, red]
      setShapes [ptShape]
      forM_ (zip ["Sarsa", "Qlearn", "ExpSarsa"] ers) $ \ (lbl, er) ->
           plot ( points lbl
                         [ (x,y)
                         | (x,y) <- takeEvery (nIters `div` 100) $ zip [(0::Int)..] er
                         ]
                )
    forM_ (zip ["Sarsa", "Qlearn", "ExpSarsa"] (P.last erss)) $ \ (lbl, er) ->
         plot ( line lbl
                       [[ (x,y)
                        | (x,y) <- zip [(0::Int)..] er
                       ]]
              )
  appendFile mdFilename "\n![](img/vFuncErr.png)  \n"
  appendFile mdFilename "circle: alpha=0.1  \n"
  appendFile mdFilename "plus: alpha=0.2  \n"
  appendFile mdFilename "line: alpha=0.5  \n"

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

