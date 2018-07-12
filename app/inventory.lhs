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

import qualified Data.Vector.Sized   as VS

import Control.Arrow
import Control.Monad.Writer
import Data.Finite
import Data.List                              (sortBy, groupBy)
-- import Data.List.Extras.Argmax                (argmax, argmin)
import Data.MemoTrie
import Data.Text                              (pack)
import Graphics.Rendering.Chart.Easy hiding   (Wrapped, Unwrapped, Empty)
import Graphics.Rendering.Chart.Backend.Cairo
import Statistics.Distribution                (density)
import Statistics.Distribution.Gamma          (gammaDistr)
-- import System.Random
import Text.Printf

import RL.GPI
\end{code}

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}

{----------------------------------------------------------------------
  Problem specific definitions
----------------------------------------------------------------------}

demandMean = 1
pDemand    = gamma' $ finite $ round demandMean

gLeadTime     =  3
gMaxOrder     =  5
gMaxOnHand    = 10
-- gReviewPer    =  1  -- `showFofState` assumes: gReviewPer = gLeadTime.
gMaxDemand    = 10
-- gProfit       =  0

-- | State data type for "single-store / single-item" inventory optimization problem.
--
-- TODO: Enforce limits, using sized vector / Finite instead of list / Int.
data MyState  = MyState
  { onHand  :: Int    -- Should lie in [-gMaxOnHand, 2*gMaxOnHand].
  , onOrder :: [Int]  -- Should have length `gLeadTime`. Elements should lie in [0, gMaxOrder].
  , epoch   :: Int    -- Needed, to determine if ordering is allowed.
  } deriving (Show, Eq, Ord, Generic)

instance HasTrie MyState where
  newtype (MyState :->: b) = MyStateTrie { unMyStateTrie :: Reg MyState :->: b } 
  trie      = trieGeneric      MyStateTrie 
  untrie    = untrieGeneric    unMyStateTrie
  enumerate = enumerateGeneric unMyStateTrie

-- TODO: Convert this to Finite (gMaxOrder + 1).
type MyAction = Int

-- | S - all possible states.
--
-- Note: There is no need to track epochs [gLeadTime..].
allStates :: [MyState]
allStates =
  [ MyState x (drop n ys ++ take n ys) n
  | x <- [-gMaxOnHand..(2 * gMaxOnHand)]
  , n <- [0..(gLeadTime - 1)]
  , y <- [0..gMaxOrder]
  , let ys = y : replicate (gLeadTime - 1) 0
  ]

-- Just a sized vector alternative to the list above.
--
-- TODO: Figure out how to determine the size from the constants above.
allStatesV :: VS.Vector 558 MyState
allStatesV = fromMaybe (P.error "main.allStatesV: Fatal error converting `allStates`!")
                       $ VS.fromList allStates

sEnum' :: MyState -> Finite 558
sEnum' MyState{..} = finite . fromIntegral $ tot where
  tot =
    if onHand < (-gMaxOnHand) || onHand > 2 * gMaxOnHand
       then P.error $ printf "sEnum': onHand out of bounds: %d" onHand
       else
         epoch `mod` gLeadTime   +
         gLeadTime * sum onOrder +
         (gLeadTime * (gMaxOrder + 1)) * (onHand + gMaxOnHand)

-- | A(s) - all possible actions from state `s`.
actions' :: MyState -> [MyAction]
actions' MyState{..} =
  -- if epoch `mod` gReviewPer /= 0  -- Until I have a more sophisticated `showFofState`.
  if epoch `mod` gLeadTime /= 0
     then [0]
     else [0..gMaxOrder]

aEnum' :: MyAction -> Finite 6
aEnum' = finite . fromIntegral

aGen' :: Finite 6 -> MyAction
aGen' = fromIntegral . getFinite

-- | S'(s, a) - list of next possible states.
nextStates' :: MyState -> MyAction -> [MyState]
nextStates' MyState{..} toOrder =
  [ MyState onHand'
            (P.tail onOrder ++ [toOrder])
            (epoch + 1)
  | demand <- [0..gMaxDemand]
  , let onHand' = onHand + P.head onOrder - demand
  -- , let onHand' = max (-gMaxOnHand) $ min (2 * gMaxOnHand) $ onHand + P.head onOrder - demand
  -- , onHand' >= -gMaxOnHand && onHand' <= 2 * gMaxOnHand
  ]

-- | Pr [(s', r) | s, a]
jointPMF :: MyState -> MyAction -> [((MyState, Double), Double)]
jointPMF MyState{..} toOrder =
  [ ((s', r), pDemand $ finite $ fromIntegral demand)
  | demand <- [0..gMaxDemand]
  , let s'       = MyState onHand'
                           (P.tail onOrder ++ [toOrder])
                           (epoch + 1)
        onHand'  = bound $ onHand + P.head onOrder - demand
        r        = fromIntegral p * fromIntegral stockOut - fromIntegral held
        held     = max 0 onHand'
        stockOut = min 0 onHand'
  ]

-- | R(s, a, s')
--
-- Returns a list of pairs, each containing:
-- - a reward value, and
-- - the probability of occurence for that value.
--
-- Note: Previous requirement that reward values be unique eliminated,
--       for coding convenience and runtime performance improvement.
rewards' :: Int -> MyState -> MyAction -> MyState -> [(Double, Double)]
rewards' p MyState{..} _ (MyState onHand' _ _) =
  [ ( fromIntegral p * fromIntegral stockOut - fromIntegral held
    , pDemand $ finite $ fromIntegral demand
    )
  | let held     = max 0 onHand'
        stockOut = min 0 onHand'
        demand   = onHand + P.head onOrder - onHand'
  ]

-- | Show a function from `MyState`, assuming the `On Order` quantity
-- is in the first element of the `onOrder` list.
--
-- This makes sense as the default, since actions (i.e. - orders) may
-- only be taken (i.e. - placed) when the first element of `onOrder` is
-- non-zero, because that corresponds to an epoch that is an integral
-- multiple of the review period, which is, currently, assumed to be
-- equal to lead time.
showFofState :: (Show a, Ord a) => (MyState -> a) -> String
showFofState = showFofState' 0

-- | Show a function from `MyState`, assuming the `On Order` quantity
-- is in the `k-th` element of the `onOrder` list.
showFofState' :: (Show a, Ord a) => Int -> (MyState -> a) -> String
showFofState' k g = unlines
  ( "\\begin{array}{" : intersperse '|' (replicate (gMaxOrder + 1) 'c') : "}" :
    ( ("\\text{On Hand} &" ++ intersperse '&' (replicate (gMaxOrder + 1) ' ') ++ " \\\\") :
      ["\\hline"] ++
      intersperse "\\hline"
        ( map ((++ " \\\\") . intercalate " & ")
              [ (show onHnd :) $ map show
                [ g (MyState onHnd (drop k ys ++ take k ys) k)
                | onOrdr <- [0..gMaxOrder]
                , let ys = onOrdr : replicate (gLeadTime - 1) 0
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
testRewards :: Int -> MyState -> Double
testRewards p s =
  mean [ ( sum
         . map (uncurry (*) . second (/ pNorm))
         ) $ rewards' p s a s'
       | a  <- acts
       , s' <- nextStates' s a
       ]
  where acts  = actions' s
        pNorm = fromIntegral $ length acts

{----------------------------------------------------------------------
  DP reference, for TD comparison.

    dca9047d694f:haskell-rl a594349$ time stack exec -- inventory --nIter 8 --nEval 4 --eps 1 --alph 0.5 --dis 0.9 --p 2

    real	27m27.109s
    user	26m0.135s
    sys	48m38.034s
----------------------------------------------------------------------}

#if 0
vDP = [ [-116.5, -99.7, -85.4, -73.9, -65.4, -59.6]
      , [ -99.7, -85.4, -73.9, -65.4, -59.6, -56.3]
      , [ -85.4, -73.9, -65.4, -59.6, -56.3, -55.3]
      , [ -73.9, -65.4, -59.6, -56.3, -55.3, -55.9]
      , [ -65.4, -59.6, -56.3, -55.3, -55.9, -57.9]
      , [ -59.6, -56.3, -55.3, -55.9, -57.9, -60.4]
      , [ -56.3, -55.3, -55.9, -57.9, -60.4, -63.0]
      , [ -55.3, -55.9, -57.9, -60.4, -63.0, -65.6]
      , [ -55.9, -57.9, -60.4, -63.0, -65.6, -68.3]
      , [ -57.9, -60.4, -63.0, -65.6, -68.3, -70.9]
      , [ -60.4, -63.0, -65.6, -68.3, -70.9, -74.1]
      ]
vDPMeanSqr :: Double
vDPMeanSqr = arrMeanSqr vDP

pDP = [ [5, 5, 5, 5, 5, 5]
      , [5, 5, 5, 5, 5, 5]
      , [5, 5, 5, 5, 5, 5]
      , [5, 5, 5, 5, 5, 5]
      , [5, 5, 5, 5, 5, 5]
      , [5, 5, 5, 5, 5, 4]
      , [5, 5, 5, 5, 4, 3]
      , [5, 5, 5, 4, 3, 2]
      , [5, 5, 4, 3, 2, 1]
      , [5, 4, 3, 2, 1, 0]
      , [4, 3, 2, 1, 0, 0]
      ]
pDPMeanSqr :: Double
pDPMeanSqr = arrMeanSqr pDP
#endif

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
    -- , alph  :: w ::: Maybe Double <?>
    --     "The error correction gain"
    , dis   :: w ::: Maybe Double <?>
        "The discount rate"
    , dcy   :: w ::: Maybe Double <?>
        "The decay rate for epsilon/alpha"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)


{----------------------------------------------------------------------
  main()
----------------------------------------------------------------------}
mdFilename = "other/inventory.md"

boolToDouble :: Bool -> Double
boolToDouble True = 1
boolToDouble _    = 0

main :: IO ()
main = do
  -- Process command line options.
  o :: Opts Unwrapped <-
    unwrapRecord "A toy inventory optimizer."
  let nIters = fromMaybe  2   (nIter o)
      nEvals = fromMaybe  1   (nEval o)
      pVal   = fromMaybe 50   (p     o)
      eps'   = fromMaybe  0.1 (eps   o)
      -- alpha' = fromMaybe  0.5 (alph  o)
      disc'  = fromMaybe  0.9 (dis   o)
      beta'  = fromMaybe  0   (dcy   o)

  -- Run DP, to generate reference values.
  let (fs, counts') = P.unzip $ take 8
                   $ iterate
                       ( optPol
                           rltDef
                             { disc       = disc'
                             , epsilon    = 1
                             , maxIter    = 4
                             , states     = allStatesV
                             , actions    = actions'
                             , nextStates = nextStates'
                             , rewards    = rewards' pVal
                             , aEnum      = aEnum'
                             }
                       ) (const (0, 0), [])
      counts = P.tail counts'
      acts   = map (\f -> VS.map (fst . f) allStatesV) fs
      diffs  = map (VS.map boolToDouble . uncurry (VS.zipWith (/=)))
                  $ zip acts (P.tail acts)
      ((_, g'), cnts') = first (fromMaybe (P.error "main: Major failure!")) $
        -- runWriter $ withinOnM eps'
        runWriter $ withinOnM 0  -- Temporary, to force `nIters` policy improvements.
                              ( \ (dv, _) ->
                                  maxAndNonZero dv
                              ) $ zip diffs (P.tail fs)
      pol    = fst . g'
      val    = snd . g'
      cnts   = cnts'

  writeFile mdFilename "\n### Final policy\n\n"
  appendFile mdFilename $ pack $ showFofState pol
  appendFile mdFilename "\n### Final value function\n\n"
  appendFile mdFilename $ pack $ showFofState (Pdouble . val)

#if 1
  -- Policy/Value changes vs. Iteration
  toFile def "img/valueDiffs_inv.png" $ do
    layout_title .= "Policy/Value Changes vs. Evaluation Iteration"
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
  appendFile "other/inventory.md" "\n![](img/valueDiffs_inv.png)\n"
#endif

  -- Run all 3 flavors of TD, comparing results to DP.
  appendFile mdFilename "\n### TD Results\n\n"

  appendFile mdFilename $ pack $ printf "epsilon = %4.2f  \n" eps'
  -- appendFile mdFilename $ pack $ printf "alpha = %3.1f  \n" alph'
  appendFile mdFilename $ pack $ printf "beta = %8.6f  \n" beta'

  -- let (erss, termValss) = unzip $ for [0.1, 0.2, 0.5] $ \ alp ->
  let (erss, _) = unzip $ for [0.1, 0.2, 0.5] $ \ alp ->
        let myRLType =
              rltDef
                { disc       = disc'
                , epsilon    = eps'
                -- , alpha      = alph'
                , alpha      = alp
                , beta       = beta'
                , maxIter    = nEvals
                , states     = allStatesV
                , actions    = actions'
                , nextStates = nextStates'
                , rewards    = rewards' pVal
                , sEnum      = sEnum'
                , aEnum      = aEnum'
                , aGen       = aGen'
                , initStates = filter ((\x -> x >= (-gMaxOnHand `div` 4) && x <= (5 * gMaxOnHand `div` 4)) . onHand) allStates
                }
            ress = for [Sarsa, Qlearn, ExpSarsa] $
                       \ stepT ->
                         doTD myRLType{tdStepType = stepT} nIters
            vss  = map (map (appV sEnum') . valFuncs) ress
            ers  = map ( map ( \ v -> (/ vRefMeanSqr) . mean $
                                      map (mean . map sqr)
                                          $ zipWith
                                              (zipWith (-))
                                              -- vDP
                                              vRef
                                              $ map (map v)
                                                    [ [ MyState onHnd
                                                                (onOrd : replicate (gLeadTime - 1) 0)
                                                                0
                                                      | onOrd <- [0..gMaxOrder]
                                                      ]
                                                    | onHnd <- [0..gMaxOnHand]
                                                    ]
                             )
                       ) vss
            vRef = map (map val)
                       [ [ MyState onHnd
                                   (onOrd : replicate (gLeadTime - 1) 0)
                                   0
                         | onOrd <- [0..gMaxOrder]
                         ]
                       | onHnd <- [0..gMaxOnHand]
                       ]
            vRefMeanSqr = arrMeanSqr vRef
         in (ers, [])
      -- nPts   = nIters * nEvals

  -- Value function error vs. Iteration
  appendFile mdFilename "\n#### Mean Square Value Function Error vs. DP\n\n"
  toFile def "img/vFuncErr_inv.png" $ do
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
  appendFile mdFilename "\n![](img/vFuncErr_inv.png)  \n"
  appendFile mdFilename "circle: alpha=0.1  \n"
  appendFile mdFilename "plus: alpha=0.2  \n"
  appendFile mdFilename "line: alpha=0.5  \n"

  -- DEBUGGING
  appendFile mdFilename "\n## debug\n\n"

  -- Reward expectations
  appendFile mdFilename "### E[reward]\n\n"
  appendFile mdFilename $ pack $ showFofState (Pdouble . testRewards pVal)

  -- Plot the pdfs.
  appendFile  "other/inventory.md"
             "\n### Demand Probability Distribution Functions\n\n"

  -- - demand PDF
  let pdf = [ (x, density (gammaDistr (1 + demandMean) 1) x)
            | x <- [0.1 * n | n <- [0..100]]
            ]
  toFile def "img/pdf.png" $
    do layout_title .= "Demand Probability Density Function (pdf)"
       setColors $ map opaque [blue, green, red, yellow]
       plot ( line "Demand Probability"
                   [ pdf ]
            )
  appendFile "other/inventory.md" "![](img/pdf.png)\n"
  appendFile "other/inventory.md" $ pack $ printf "\n$\\int pdf = %5.2f$\n" (0.1 * sum (map snd pdf))

  -- - demand PMF
  let pmf = [ (x, gamma' (finite $ round demandMean) (finite x))
            | x <- [0..10]
            ]
      titles = ["pmf"]
      values :: [ (String,[Double]) ]
      values = map (show *** (: [])) pmf
  toFile def "img/demand.png" $
    do layout_title .= "Demand Probability Mass Function (pmf)"
       setColors $ map opaque [blue, green, red, yellow]
       layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
       plot $ plotBars <$> bars titles (addIndexes (map snd values))
  appendFile "other/inventory.md" "\n![](img/demand.png)\n"
  appendFile "other/inventory.md" $ pack $ printf "\n$\\sum pmf = %5.2f$\n" (sum $ map snd pmf)

  -- - next state PMF
  let nxtStPMFs :: [[(MyState, Double)]]
      nxtStPMFs = map ( map (fst . P.head &&& sum . map snd)
                      . groupBy ((==) `on` fst)
                      . sortBy (compare `on` fst)
                      . ( \ st ->
                            [ (nxtSt, prob / fromIntegral (length (actions' st)))
                            | act   <- actions' st
                            , nxtSt <- nextStates' st act
                            , let prob = (sum . map snd) $ rewards' pVal st act nxtSt
                            ]
                        )
                      ) allStates
      pmfSums :: [Double]
      pmfSums = map (sum . map snd) nxtStPMFs
  appendFile "other/inventory.md" $ pack $ printf "\nNext state PMF sums: min = %5.2f; max = %5.2f.\n"
                                                  (minimum pmfSums) (maximum pmfSums)

#if 0
  -- Debugging info from `doTD`
  -- data Dbg s = Dbg
  --   { nxtStPrbs :: [(s, Double)]
  --   , curSt     :: s
  --   , nxtSt     :: s
  --   , rwd       :: Double
  --   , eNxtVal   :: Double
  --   , randGen   :: g
  --   } deriving (Show)
  appendFile mdFilename "\n### Debugging info from doTD\n\n"
  appendFile mdFilename $ pack $ printf "Length dbgss: %d  \n" (length dbgss)
  appendFile mdFilename $ pack $ printf "Length dbgss[0]: %d  \n" (length $ P.head dbgss)
  let dbgs       = concat dbgss
      nxtStPrbss = map nxtStPrbs dbgs
      nxtStTots  = map (sum . map snd) nxtStPrbss
      rwds       = map rwd dbgs
      eNxtVals   = map eNxtVal dbgs
      curSts     = map curSt dbgs
      randGens   = map randGen dbgs
  appendFile mdFilename $ pack $ printf "Next state total probabilities: min. = %f, max. = %f  \n" (minimum nxtStTots) (maximum nxtStTots)
  appendFile mdFilename $ pack $ printf "Rewards: min. = %f, max. = %f  \n" (minimum rwds) (maximum rwds)
  appendFile mdFilename $ pack $ printf "E[Q(s', a')]: min. = %f, max. = %f  \n" (minimum eNxtVals) (maximum eNxtVals)

  -- Initial state histogram
  let titles' = ["First state visits"]
      values' = map (show *** (: [])) $
                    map (P.head &&& length) $
                        group . sort $ map (getFinite . sEnum') curSts
  toFile def "img/stHist.png" $
    do layout_title .= "First State Histogram"
       setColors $ map opaque [blue, green, red, yellow]
       layout_x_axis . laxis_generate .= autoIndexAxis (map fst values')
       plot $ plotBars <$> bars titles' (addIndexes (map snd values'))
  appendFile "other/inventory.md" "\n![First state histogram](img/stHist.png)\n"

  -- Random number generator checking
  let rns = map (fst . random) randGens :: [Integer]
  let titles'' = ["Random Numbers"]
      values'' = map (show *** (: [])) $
                    map (P.head &&& length) $
                        group . sort $ rns
  toFile def "img/randNums.png" $
    do layout_title .= "Random Numbers"
       setColors $ map opaque [blue, green, red, yellow]
       layout_x_axis . laxis_generate .= autoIndexAxis (map fst values'')
       plot $ plotBars <$> bars titles'' (addIndexes (map snd values''))
  appendFile "other/inventory.md" "\n![Random numbers](img/randNums.png)\n"


#endif

#if 0
doDP
  :: RLType MyState MyAction 378 6
  -> Int
  -> (MyState -> Double, MyState -> MyAction, [Int], [[Int]])
doDP rlt nIters =
  let (fs, counts') = P.unzip $ take (nIters + 1) $
        iterate
          (optPol rlt)
          (const (0, 0), [])
      valChngCnts = P.tail counts'
      acts        = map (\f -> VS.map (fst . f) allStatesV) fs
      diffs       = map ( VS.map (fromIntegral . abs)
                        . uncurry (-)
                        ) $ zip acts $ P.tail acts
      ((_, g'), polChngCnts) = first (fromMaybe (P.error "main: Major failure!")) $
        -- runWriter $ withinOnM eps'
        runWriter $ withinOnM 0  -- Temporary, to force `nIters` policy improvements.
                              ( \ (dv, _) ->
                                  maxAndNonZero dv
                              ) $ zip diffs (P.tail fs)
      pol   = fst . g'
      val   = snd . g'
   in (val, pol, polChngCnts, valChngCnts)
#endif

for = flip map

takeEvery _ [] = []
takeEvery n xs = P.head xs : (takeEvery n $ drop n xs)

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

