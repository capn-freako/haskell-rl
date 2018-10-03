```include
other/header.md
```

haskell-rl : Blackjack Problem (Example 5.3)
===

This [literate Haskell](https://wiki.haskell.org/Literate_programming) document provides a solution to Example 5.3 in _Reinforcement Learning_ by Sutton & Barto.

Original author: [David Banas](mailto:David.Banas@target.com)  
Original date:   May 17, 2018

Copyright &copy; 2018 Target Corp.; all rights reserved World wide.

Contents
---

- [Code](#code)
- [Output](#output)

code
---

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -cpp #-}
\end{code}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

\begin{code}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
\end{code}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)
- [vector-sized](https://www.stackage.org/package/vector-sized)
- [finite-typelits](https://www.stackage.org/package/finite-typelits)
- [Chart](https://www.stackage.org/package/Chart)
- [Chart-cairo](https://www.stackage.org/package/Chart-cairo)

\begin{code}
import qualified Prelude as P
import Prelude (unlines, Show(..), String)

import Protolude  hiding (show, for)
import Options.Generic

import qualified Data.Vector.Sized as VS

-- import Control.Arrow          ((***), (&&&))
import Data.MemoTrie
import Data.Finite
import Data.Finite.Internal
import Data.Text              (pack)
import Data.Vector.Sized      (Vector, index)
import Graphics.Rendering.Chart.Easy hiding   (Wrapped, Unwrapped, Empty, Iso, Vector, index)
import Graphics.Rendering.Chart.Backend.Cairo
import Text.Printf

import ConCat.TArr
import ConCat.Isomorphism

import RL.Markov
import RL.GPI
import RL.Util
\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}

{----------------------------------------------------------------------
  State definitions & instances.
----------------------------------------------------------------------}

type Nstates = 440

data BJState = BJState
  { playerSum  :: Finite 11  -- ^ Represents the range: 12 - 22
  , usableAce  :: Bool       -- ^ A "usable" ace is one currently counted as 11.
  , dealerCard :: Finite 10  -- ^ "0" is an ace; all face cards are "9".
  , done       :: Bool       -- ^ Flags a game as complete when true.
  } deriving (Show, Eq, Ord, Generic)

instance HasFin BJState where
  type Card BJState = Nstates
  iso = Iso stToFin finToSt

stToFin :: BJState -> Finite Nstates
stToFin BJState{..} = finite $
  getFinite playerSum     * 40 +
  boolToInteger usableAce * 20 +
  getFinite dealerCard    * 2 +
  boolToInteger done
  
finToSt :: Finite Nstates -> BJState
finToSt n =
  let (pSum,  r)    = divMod (getFinite n) 40
      (ace,   r')   = divMod r 20
      (dCard, done) = divMod r' 2
   in if pSum > 10
         then P.error "finToSt: Blew out a Finite 11!"
         else BJState (finite pSum) (integerToBool ace) (finite dCard) (integerToBool done)

instance HasTrie BJState where
  newtype (BJState :->: b) = BJStateTrie { unBJStateTrie :: Reg BJState :->: b } 
  trie      = trieGeneric      BJStateTrie 
  untrie    = untrieGeneric    unBJStateTrie
  enumerate = enumerateGeneric unBJStateTrie

-- | The @'MDP'@ instance for @'BJState'@.
instance IsState BJState where
  states     = allStates
  termStates = filter (\BJState{..} -> done == True)                   allStates
  initStates = filter (\BJState{..} -> done /= True && playerSum < 10) allStates

instance MDP BJState where
  type ActionT BJState = BJAction
  actions = \BJState{..} ->
    if playerSum > 8 || done == True  -- If we're made or bust then we have to stand.
      then [Stand]
      else [Hit, Stand]
  jointPMF st = \case
    Hit -> hit   st
    _   -> stand st

allStates :: [BJState]
allStates =
  [ BJState (finite pSum) ace (finite dlrCrd) dn
  | pSum   <- [0..10]
  , ace    <- [False, True]
  , dlrCrd <- [0..9]
  , dn     <- [False, True]
  ]

stateDef :: BJState
stateDef = BJState
  { playerSum  = finite 0
  , usableAce  = False
  , dealerCard = finite 9
  , done       = False
  }

{----------------------------------------------------------------------
  Actions
----------------------------------------------------------------------}

type Nacts = 2

data BJAction = Hit
              | Stand
  deriving (Eq, Ord, Generic)

instance Show BJAction where
  show = \case
    Hit -> "H"
    _   -> " "

instance HasTrie BJAction where
  newtype (BJAction :->: b) = BJActionTrie { unBJActionTrie :: Reg BJAction :->: b } 
  trie      = trieGeneric      BJActionTrie 
  untrie    = untrieGeneric    unBJActionTrie
  enumerate = enumerateGeneric unBJActionTrie

instance HasFin BJAction where
  type Card BJAction = Nacts
  iso = Iso actToFin finToAct

actToFin :: BJAction -> Finite Nacts
actToFin = \case
  Hit -> 0
  _   -> 1

finToAct :: Finite Nacts -> BJAction
finToAct = \case
  0 -> Hit
  _ -> Stand

{----------------------------------------------------------------------
  Helper functions.
----------------------------------------------------------------------}

hit :: BJState -> [((BJState, Double), Double)]
hit st@BJState{..} =
  [ ((nxtSt, 0), 0.0769)           -- 0.0769 = 1/13.
  | card <- [1..10] ++ [10,10,10]  -- Ten, Jack, Queen, and King = 10.
  , let (tot, ace) = cardSum (getFinite playerSum + 12) usableAce card
        nxtSt      = st{ playerSum = finite $ min 10 (tot - 12)
                       , usableAce = ace
                       }
  ]

stand :: BJState -> [((BJState, Double), Double)]
stand st@BJState{..} =
  [ ((st', -1), 1 - pWin - pDraw)
  , ((st',  0), pDraw)
  , ((st',  1), pWin)
  ]
  where
    st'  = st{done = True}
    pWin =
      if playerSum == 10         -- Player went bust.
        then 0
        else dlrTotPmfV `VS.index` dealerCard `VS.index` (finite 5)  -- P[Dealer went bust]
           + if playerSum > 5  -- Player's total > 17?
               then dlrTotCmfV `VS.index` dealerCard `VS.index` (finite (getFinite playerSum - 6))
               else 0
    pDraw =
      if playerSum > 4 && playerSum < 10  -- Player's total > 16 and < 22?
        then dlrTotPmfV `VS.index` dealerCard `VS.index` (finite (getFinite playerSum - 5))
        else 0

-- | Adjust the total count of a hand after taking a card.
--
-- Make use of a "usable ace" to avoid going bust.
-- (A "usable ace" is one that is currently being counted as 11.)
cardSum :: Integer -> Bool -> Integer -> (Integer, Bool)
cardSum acc ace card = (acc', ace')
  where (acc', ace') = case card of
          1 -> case ace of
            True -> if acc + 1 > 21
                      then (acc - 9, False)  -- Use our usable ace to avoid bust.
                      else (acc + 1, True)
            _    -> if acc + 11 <= 21
                      then (acc + 11, True)  -- Count the ace as 11.
                      else (acc +  1, False)
          _ -> case ace of
            True -> if acc + card > 21
                      then (acc + card - 10, False)
                      else (acc + card,      True)
            _    -> (acc + card, False)

-- | Array of dealer total probabilities.
--
-- Rows are indexed by dealer's up card (0 = ace).
-- Columns are indexed by hand total (0 = 17, 5 = bust).
dlrTotPmfV :: Vector 10 (Vector 6 Double)
dlrTotPmfV = VS.generate $ \r ->
  fromMaybe (P.error "dlrTotPmfV: VS.fromList failure!")
            $ VS.fromList $ map snd $ dlrTots r
    
-- | Array of dealer total cumulative probabilities.
--
-- Rows are indexed by dealer's up card (0 = ace).
-- Columns are indexed by hand total (0 = 17, 5 = bust).
dlrTotCmfV :: Vector 10 (Vector 6 Double)
dlrTotCmfV = VS.generate $ \r ->
  VS.postscanl' (+) 0 $ dlrTotPmfV `VS.index` r
    
-- | Probabilities of dealer total vs. up card.
dlrTots :: Finite 10 -> [(Integer, Double)]
dlrTots (Finite n) = sortOn fst $ combProb $ map (first (min 22)) tots
 where
  tots = case n of
    0 -> playHand 11      True  1
    _ -> playHand (n + 1) False 1
  
-- | Play out the dealer's hand.
--
-- Take as input:
--   - the current total,
--   - a flag indicating a usable ace, and
--   - the accumulated probability so far.
--
-- Return a list of pairs, each containing the final tally and its probability.
playHand :: Integer -> Bool -> Double -> [(Integer, Double)]
playHand tot ace p =
  if tot > 16
    then [(tot, p)]
    else concat [ playHand tot' ace' (p/13)
                | card <- [1..10] ++ [10,10,10]
                , let (tot', ace') = cardSum tot ace card
                ]

showFofState :: Show a => (BJState -> a) -> String
showFofState f = intercalate "\n\n" $ map (showFofState' f) [True, False]

showFofState' :: Show a => (BJState -> a) -> Bool -> String
showFofState' f ace = unlines $
  (if ace
     then "With usable ace:"
     else "No usable ace:")
  : ( "\\begin{array}{c|c|c|c|c|c|c|c|c|c}" :
      ( ("\\text{Player's Total} &" ++ intersperse '&' (replicate 10 ' ') ++ " \\\\") :
        ["\\hline"] ++
        intersperse "\\hline"
          ( map ((++ " \\\\") . intercalate " & ")
                [ (show pTot :) $ map show
                  [ f s
                  | dCard <- [1..10]
                  , let s = stateDef { playerSum  = finite $ pTot - 12
                                     , usableAce  = ace
                                     , dealerCard = finite $ dCard - 1
                                     }
                  ]
                | pTot <- [12..21]
                ]
          )
        ++ ["\\hline"]
        ++ [intercalate " & " $ "\\text{Dealer's Card:} " : [show n | n <- [1..10]]]
        ++ ["\\end{array}"]
      )
    )

showVofState :: (Show a) => Vector Nstates a -> String
showVofState v = intercalate "\n\n" $ map (showVofState' v) [True, False]

showVofState' :: Show a => Vector Nstates a -> Bool -> String
showVofState' v ace = unlines $
  (if ace
     then "With usable ace:"
     else "No usable ace:")
  : ( "\\begin{array}{c|c|c|c|c|c|c|c|c|c}" :
      ( ("\\text{Player's Total} &" ++ intersperse '&' (replicate 10 ' ') ++ " \\\\") :
        ["\\hline"] ++
        intersperse "\\hline"
          ( map ((++ " \\\\") . intercalate " & ")
                [ (show pTot :) $ map show
                  [ v `index` toFin s
                  | dCard <- [1..10]
                  , let s = stateDef { playerSum  = finite $ pTot - 12
                                     , usableAce  = ace
                                     , dealerCard = finite $ dCard - 1
                                     }
                  ]
                | pTot <- [12..21]
                ]
          )
        ++ ["\\hline"]
        ++ [intercalate " & " $ "\\text{Dealer's Card:} " : [show n | n <- [1..10]]]
        ++ ["\\end{array}"]
      )
    )

showGame :: [Dbg BJState BJAction Double] -> String
showGame [] = "\n"
showGame (x : xs) = showStep x ++ (showGame xs)

showStep :: Dbg BJState BJAction Double -> String
showStep Dbg{..} =
  printf "Total: %2d, Action: %s, Reward: %3.1f  \n"            
         (12 + (getFinite . playerSum) curSt)
         (if act == Hit then ("H" :: String) else "S")
         (rwd)

{----------------------------------------------------------------------
  Command line options defintions.
----------------------------------------------------------------------}

data Opts w = Opts
    { nGames  :: w ::: Maybe Int <?>
        "The number of games to play."
    , eps     :: w ::: Maybe Double <?>
        "Epsilon in epsilon-greedy."
    , dcy     :: w ::: Maybe Double <?>
        "The decay rate for epsilon"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)


{----------------------------------------------------------------------
  main()
----------------------------------------------------------------------}

mdFilename   = "other/blackjack.md"
plotFilename = "img/blackjackPlot.png"

main :: IO ()
main = do
  -- Process command line options.
  o :: Opts Unwrapped <-
    unwrapRecord "A solution to the 'Blackjack' problem (Example 5.3)."
  let nG     = fromMaybe 10000     (nGames o)
      eps'   = fromMaybe     0.1   (eps    o)
      beta'  = fromMaybe     0     (dcy    o)

#if 1
  -- Apply DP.
  let DPRetT{..} =
        doDP
          hypParamsDef
            { maxIter = 10  -- 10 policy evaluation iterations per improvement iteration.
            }
          nG

  -- Output the results.
  writeFile  mdFilename "\n### Final Policy (H = hit)\n\n"
  appendFile mdFilename $ pack $ showFofState $ polFunc

  appendFile mdFilename "\n### Final Value Function\n\n"
  appendFile mdFilename $ pack $ showFofState $ Pdouble . valFunc
  
  -- Value/Action changes vs. Iteration
  appendFile mdFilename "\n#### Policy/Value Function Changes vs. Iteration\n\n"
  toFile def plotFilename $ do
    layout_title .= "Policy/Value Function Changes vs. Iteration"
    setColors $ map opaque [blue, green, red, yellow, cyan, magenta, brown, gray, purple, black]
    plot ( line "Policy Changes"
                [ [ (x,y)
                  | (x,y) <- zip (map (* (length $ P.head valXCnts)) [(0::Int)..])
                                 polXCnts'
                  ]
                ]
         )
    plot ( line "Value Changes"
                [ [ (x,y)
                  | (x,y) <- zip [(0::Int)..]
                                 (concat valXCnts)
                  ]
                ]
         )
  appendFile mdFilename $ pack $ printf "\n![](%s)\n" plotFilename

#else
  -- Play many games, updating policy and state/action values after each.
  -- (results, _) <- runWriterT $ iterateM' nG optPol (qs, pol)
  let myHypParams =
        hypParamsDef
          { tdStepType = Qlearn
          , epsilon    = eps'
          , beta       = beta'
          , maxIter    = 1       -- Because we're using Monte Carlo (i.e. - ending in a terminal state).
          , nSteps     = 10      -- No game should require more than 10 hits.
          , mode       = MC      -- Monte Carlo
          }
      -- ress :: [TDRetT MyState MyAction Double (Card MyState) (Card (ActionT MyState))]
      res  = doTD myHypParams nG
      qMat = P.last $ qMats  res
      maxN = maximum $ map length $ debugs res
      dbgs = P.head $ filter ((== maxN) . length) $ debugs res
      tsts :: [[Dbg BJState BJAction Double]]
      tsts =
        filter
          (any ( \dbg ->
                   let s = curSt dbg
                    in playerSum  s == 0      -- 12
                    && usableAce  s == False
                    && dealerCard s == 5      -- 6
                    && act dbg      == Hit
               )
          ) $ debugs res
      tstsTotRwd  = sum $ map (rwd . P.last) tsts
      tstsMeanRwd = tstsTotRwd / (fromIntegral . length) tsts
      tstsIncrements =
        map ( getFinite . playerSum . curSt
            . P.head . P.tail
            . dropWhile ( \x ->
                            playerSum (curSt x) /= 0 ||
                            usableAce (curSt x) /= False
                        )
            ) tsts

  -- Output the results.
  writeFile  mdFilename "\n### Final Policy (H = hit)\n\n"
  appendFile mdFilename $ pack $ showFofState $ P.last $ polFuncs res

  -- Value/Action changes vs. Iteration
  appendFile mdFilename "\n### Policy/Value Function Changes vs. Iteration\n\n"
  toFile def plotFilename $ do
    layoutlr_title .= "Policy/Value Function Changes vs. Iteration"
    setColors $ map opaque [blue, red, green, yellow, cyan, magenta, brown, gray, purple, black]
    plotLeft  ( line "Policy Changes"
                 [ [ (x,y)
                   | (x,y) <- zip [(0::Int)..]
                                  ((winMean 100 $ map fromIntegral $ polXCnts res) :: [Double])
                   ]
                 ]
              )
    plotRight ( line "Value Changes"
                [ [ (x,y)
                  | (x,y) <- zip [(0::Int)..]
                                 (winMean 100 $ valErrs res)
                  ]
                ]
              )
  appendFile mdFilename $ pack $ printf "\n![](%s)\n" plotFilename

  -- Debugging Info
  appendFile mdFilename "\n### Debugging Info\n\n"

  appendFile mdFilename $ pack $ printf "**Number of games played:** %d  \n" nG
  appendFile mdFilename $ pack $ printf "**Epsilon:** %f  \n" eps'
  appendFile mdFilename $ pack $ printf "**Beta:** %f\n\n" beta'

  -- appendFile mdFilename "**12/6 game sequences:**\n\n"
  -- appendFile mdFilename $ pack $ concat $ map showGame tsts

  appendFile mdFilename $ pack $ printf
    "**Mean reward from hitting hard 12 w/ dealer showing 6:** %4.2f\n\n" tstsMeanRwd

  appendFile mdFilename
    "**# of occurences of drawn card when hitting hard 12 when dealer shows a 6:**\n\n"
  appendFile mdFilename
    "| Card: | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |\n"
  appendFile mdFilename
    "| -----:| - | - | - | - | - | - | - | - | - | -- |\n"
  appendFile mdFilename
    "|    #: | "
  appendFile mdFilename $ pack $ intercalate " | "
    [ show n
    | x <- [1..10]
    , let n = length $ filter (== x) tstsIncrements
    ]
  appendFile mdFilename " |\n\n"

  appendFile mdFilename "**Longest game sequence:**  \n"
  appendFile mdFilename $ pack $ unlines $
    map ( \Dbg{..} ->
            printf "\tTotal: %2d, Action: %s, Reward: %3.1f  "            
                   (12 + (getFinite . playerSum) curSt)
                   (if act == Hit then ("H" :: String) else "S")
                   (rwd)
        ) dbgs
      
  appendFile mdFilename $ pack $ printf "\n**Max. game length:** %d\n\n" maxN

  appendFile  mdFilename "**Final State Action Values (stand, hit):**\n\n"
  appendFile mdFilename $ pack $ showVofState
    $ VS.map (((Pdouble . fst) *** (Pdouble . fst)) . ((`index` 1) &&& (`index` 0))) qMat

  appendFile mdFilename "\n**State Action Visits (stand, hit):**\n\n"
  appendFile mdFilename $ pack $ showVofState
    $ VS.map ((snd *** snd) . ((`index` 1) &&& (`index` 0))) qMat

  -- appendFile mdFilename "\n**Testing State Enumeration:**\n\n"
  -- appendFile mdFilename $ pack $ showFofState toFin

  appendFile mdFilename "\n**Probability of Dealer Total vs. Up Card:**\n\n"
  appendFile mdFilename $ pack $ showFofState $ \BJState{..} ->
    let tots  = dlrTots dealerCard
        targs = filter ((== (getFinite playerSum + 12)) . fst) tots
     in Pdouble $ if length targs > 0
                    then snd $ P.head $ targs
                    else 0

  appendFile mdFilename "\n**Contents of Dealer Total PMF vs. Up Card:**\n\n"
  appendFile mdFilename $ pack $ showFofState $ \BJState{..} ->
      Pdouble $ if playerSum > 4
                    then dlrTotPmfV `VS.index` dealerCard
                                    `VS.index` (finite (getFinite playerSum - 5))
                    else 0

  appendFile mdFilename "\n**Contents of Dealer Total CMF vs. Up Card:**\n\n"
  appendFile mdFilename $ pack $ showFofState $ \BJState{..} ->
      Pdouble $ if playerSum > 4
                    then dlrTotCmfV `VS.index` dealerCard
                                    `VS.index` (finite (getFinite playerSum - 5))
                    else 0

#endif

\end{code}

output
---

```include
other/blackjack.md
```

***

<div class="footer">
Powered by [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/) and [pandoc](http://pandoc.org/).
</div>

