```include
other/header.md
```

haskell-rl : Inventory Optimization for Single Store, Single Item Case
===

This [literate Haskell](https://wiki.haskell.org/Literate_programming) document provides a solution to ordering optimization for the "toy" case of a single store selling a single item.

Original author: [David Banas](mailto:David.Banas@target.com)  
Original date:   June 14, 2018

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
import Protolude  hiding (show, for)

import Options.Generic

import Control.Arrow                        ((***))
import Control.Monad.Writer
import qualified Data.Vector.Sized   as VS
import Data.Finite
import Data.MemoTrie
import Data.Text                            (pack)
import Graphics.Rendering.Chart.Easy hiding (Wrapped, Unwrapped, Empty)
import Graphics.Rendering.Chart.Backend.Cairo
import Statistics.Distribution       (density)
import Statistics.Distribution.Gamma (gammaDistr)
import Text.Printf

import RL.GPI
\end{code}

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}
{----------------------------------------------------------------------
  Problem specific definitions
----------------------------------------------------------------------}

eps'  = 10    -- my choice
disc' = 0.9  -- my choice

demand_mean = 1
pDemand = gamma' $ finite $ round demand_mean

gLeadTime     =  3
gMaxOrder     =  5
gMaxOnHand    = 10
-- gReviewPer    =  1  -- `showFofState` assumes: gReviewPer = gLeadTime.
gMaxDemand    = 10
gProfit       =  0
gHoldingCost  =  1
gStockOutCost =  2

data MyState  = MyState
  { onHand  :: Int
  , onOrder :: [Int]
  , epoch   :: Int
  } deriving (Show, Eq, Generic)

instance HasTrie MyState where
  newtype (MyState :->: b) = MyStateTrie { unMyStateTrie :: Reg MyState :->: b } 
  trie      = trieGeneric      MyStateTrie 
  untrie    = untrieGeneric    unMyStateTrie
  enumerate = enumerateGeneric unMyStateTrie

type MyAction = Int

-- | S
allStates :: [MyState]
allStates =
  [ MyState x (drop n ys ++ take n ys) n
  | x <- [0..gMaxOnHand]
  , n <- [0..(gLeadTime - 1)]
  , y <- [0..gMaxOrder]
  , let ys = y : (replicate (gLeadTime - 1) 0)
  ]

-- Just a sized vector alternative to the list above.
allStatesV :: VS.Vector 198 MyState
allStatesV = fromMaybe (P.error "main.allStatesV: Fatal error converting `allStates`!")
                       $ VS.fromList allStates

-- | A(s)
actions' :: MyState -> [MyAction]
actions' MyState{..} =
  -- if epoch `mod` gReviewPer /= 0  -- Until I have a more sophisticated `showFofState`.
  if epoch `mod` gLeadTime /= 0
     then [0]
     else [0..gMaxOrder]

-- | S'(s, a)
nextStates' :: MyState -> MyAction -> [MyState]
nextStates' MyState{..} toOrder =
  [ MyState (onHand + P.head onOrder - sold)
            (P.tail onOrder ++ [toOrder])
            (epoch + 1)
  | sold <- [0..onHand]
  ]

-- | R(s, a, s')
--
-- Returns a list of pairs, each containing:
-- - a reward value, and
-- - the probability of occurence for that value.
--
-- Note: Previous requirement that reward values be unique eliminated,
--       for coding convenience and runtime performance improvement.
rewards' :: MyState -> MyAction -> MyState -> [(Double, Double)]
rewards' MyState{..} toOrder (MyState onHand' _ _) =
  [ ( gProfit * fromIntegral sold
      - gHoldingCost  * fromIntegral onHand'
      - gStockOutCost * fromIntegral missedSales
    , pDemand (finite $ fromIntegral demand1)
    )
  | let demands = if onHand' == P.head onOrder
                     then [ (x,                                 x)
                          | x <- [0..gMaxDemand]
                          ]
                     else [ (onHand + P.head onOrder - onHand', x)
                          | x <- [0..gMaxDemand]
                          ]
  , (demand0, demand1) <- demands
  , let totAvailable = onHand + sum onOrder + toOrder
        totDemand    = demand0 + (gLeadTime + 1) * demand1
        sold         = min totAvailable totDemand
        missedSales  = max 0 (totDemand - totAvailable)
  ]

-- NOTE: Please, keep the following commentary, as it explains the
-- above code.
--
-- onHand' = onHand + head onOrder - sold
--         = onHand + head onOrder - min onHand demand
--
-- demand >= onHand:
--   onHand' = head onOrder
--   Demand may have exceeded amount on hand; we can't know for sure.
--   => Allow demand to sweep entire range.
--
-- otherwise:
--   onHand' = onHand + head onOrder - demand
--   demand  = onHand + head onOrder - onHand'
--   => Demand is deterministic.
--
-- NOTE: In order to avoid exponential state space explosion, I'm
--       assuming that demand is constant for all days of interest
--       after the first, which is determined according to the
--       commentary above.


-- | Show a function from `MyState` as a table.
--
-- Note: Naively assumes that lead time = review period.
showFofState :: (Show a) => (MyState -> a) -> String
showFofState g = unlines
  ( "\\begin{array}{" : intersperse '|' (replicate (gMaxOrder + 1) 'c') : "}" :
    ( ("\\text{On Hand} &" ++ intersperse '&' (replicate (gMaxOrder + 1) ' ') ++ " \\\\") :
      ["\\hline"] ++
      intersperse "\\hline"
        ( map ((++ " \\\\") . intercalate " & ")
              [ (show onHnd :) $ map show
                [ g (MyState onHnd (onOrdr : (replicate (gLeadTime - 1) 0)) 0)
                | onOrdr <- [0..gMaxOrder]
                ]
              | onHnd' <- [0..gMaxOnHand]
              , let onHnd = gMaxOnHand - onHnd'
              ]
        )
      ++ ["\\hline"]
      ++ [intercalate " & " $ "\\text{On Order:} " : [show n | n <- [0..gMaxOrder]]]
      ++ ["\\end{array}"]
    )
  )

-- | Expected reward for a given state, assuming equiprobable actions.
testRewards :: MyState -> Double
testRewards s =
  mean [ (sum . map (uncurry (*))) $ rewards' s a s'
       | a  <- actions' s
       , s' <- nextStates' s a
       ]

{----------------------------------------------------------------------
  Command line options defintions.
----------------------------------------------------------------------}

data Opts w = Opts
    { nIter :: w ::: Maybe Int <?>
        "The number of policy improvement iterations"
    , nEval :: w ::: Maybe Int <?>
        "The number of policy evaluation iterations per policy improvement iteration"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)


{----------------------------------------------------------------------
  main()
----------------------------------------------------------------------}

main :: IO ()
main = do
  -- Process command line options.
  o :: Opts Unwrapped <-
    unwrapRecord "A toy inventory optimizer."
  let nIters = fromMaybe 2 (nIter o)
      nEvals = fromMaybe 1 (nEval o)

  -- Calculate and display optimum policy.
  writeFile "other/inventory.md" "\n### Policy optimization\n\n"
  let (fs, counts) = unzip $ P.tail $ take (nIters + 1)
                   $ iterate
                       ( optPol
                           rltDef
                             { disc       = disc'
                             , epsilon    = eps'
                             , maxIter    = nEvals
                             , states     = allStatesV
                             , actions    = actions'
                             , nextStates = nextStates'
                             , rewards    = rewards'
                             }
                       ) (const (0,0), [])
      acts  = map (\f -> VS.map (fst . f) allStatesV) fs
      diffs = map (VS.map (fromIntegral . abs) . uncurry (-))
                  $ zip acts (P.tail acts)
      ((_, g'), cnts) = first (fromMaybe (P.error "main: Major failure!")) $
        -- runWriter $ withinOnM eps'
        runWriter $ withinOnM 0
                              ( \ (dv, _) ->
                                  maxAndNonZero dv
                              ) $ zip diffs (P.tail fs)
      pol   = fst . g'
      val   = snd . g'
  appendFile "other/inventory.md" "\n### Final policy\n\n"
  appendFile "other/inventory.md" $ pack $ showFofState pol
  appendFile "other/inventory.md" "\n### Final value function\n\n"
  appendFile "other/inventory.md" $ pack $ showFofState (Pdouble . val)

  -- DEBUGGING
  appendFile "other/inventory.md" "\n## debug\n\n"

  -- Reward expectations
  appendFile "other/inventory.md" "\n### E[reward]\n\n"
  appendFile "other/inventory.md" $ pack $ showFofState (Pdouble . testRewards)

  -- Value/Action changes vs. Iteration
  toFile def "img/valueDiffs.png" $ do
    layout_title .= "Value Changes vs. Evaluation Iteration"
    setColors $ map opaque [blue, green, red, yellow, cyan, magenta, brown, gray, purple, black]
    forM_ ( zip counts
                $ map (printf "Imp. Iter. #%d") [(1::Int)..]
          ) $ \ (count, lbl) ->
                plot ( line lbl
                            [ [ (x, y)
                              | (x, y) <- zip [(0::Int)..] count
                              ]
                            ]
                     )
  appendFile "other/inventory.md" "\n![](img/valueDiffs.png)\n"

  toFile def "img/actionDiffs.png" $
    do layout_title .= "Action Changes vs. Improvement Iteration"
       setColors $ map opaque [blue, green, red, yellow]
       plot ( line "Action Changes"
                   [ [ (x, y)
                     | (x, y) <- zip [(0::Int)..] cnts
                     ]
                   ]
            )
  appendFile "other/inventory.md" "\n![](img/actionDiffs.png)\n"

  -- Plot the pdfs.
  appendFile  "other/inventory.md"
             "\n### Demand Probability Distribution Functions\n\n"
  let pdf = [ (x, density (gammaDistr (1 + demand_mean) 1) x)
            | x <- [0.1 * n | n <- [0..100]]
            ]
  toFile def "img/pdf.png" $
    do layout_title .= "Demand Probability Density Function (pdf)"
       setColors $ map opaque [blue, green, red, yellow]
       plot ( line "Demand Probability"
                   [ pdf ]
            )
  let pmf = [ (x, gamma' (finite $ round demand_mean) (finite x))
            | x <- [0..10]
            ]
      titles = ["pmf"]
      values :: [ (String,[Double]) ]
      values = map (show *** (: [])) pmf
  
  toFile def "img/demand.png" $
    do layout_title .= "Demand Probability Mass Function (pmf)"
       setColors $ map opaque [blue, green, red, yellow]
       layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
       plot $ fmap plotBars $ bars titles (addIndexes (map snd values))
  appendFile "other/inventory.md" "![](img/pdf.png)\n"
  appendFile "other/inventory.md" $ pack $ printf "\n$\\int pdf = %5.2f$\n" (0.1 * (sum $ map snd pdf))
  appendFile "other/inventory.md" "\n![](img/demand.png)\n"
  appendFile "other/inventory.md" $ pack $ printf "\n$\\sum pmf = %5.2f$\n" (sum $ map snd pmf)

\end{code}

output
---

```include
other/inventory.md
```

***

<div class="footer">
Powered by [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/) and [pandoc](http://pandoc.org/).
</div>

