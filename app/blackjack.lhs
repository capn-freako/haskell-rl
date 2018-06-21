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
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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
import Prelude (unlines, Show(..), String, error, (!!))

import Protolude  hiding (show, for, error)
import Options.Generic

import qualified Data.Vector.Sized as VS

import Control.Arrow          ((&&&), (***))
import Control.Monad.Writer
import Data.Vector.Sized      (Vector, (//), index)
import Data.Finite
import Data.Text              (pack)
import System.Random.Shuffle
import Text.Printf
import Useful.IO              (rand)
\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}

{----------------------------------------------------------------------
  Problem specific definitions
----------------------------------------------------------------------}

data BJState = BJState
  { playerSum  :: Int   -- 12 - 21
  , usableAce  :: Bool
  , dealerCard :: Int   -- 1 - 10
  , dealerSum  :: Int   -- For debugging only; not used for learning.
  , polHit     :: Bool  -- For debugging only; not used for learning.
  , didHit     :: Bool
  } deriving (Show)

stateDef :: BJState
stateDef = BJState
  { playerSum  = 12
  , usableAce  = False
  , dealerCard = 1
  , dealerSum  = 12
  , polHit     = False
  , didHit     = False
  }

newtype BJAction = BJAction { act :: Bool }  -- Hit?

instance Show BJAction where
  show (BJAction x) =
    if x then "H"
         else " "

allActs :: [BJAction]
allActs = map BJAction [False, True]

type BJPolicy = BJState -> BJAction

appPolV :: Vector 200 BJAction -> BJPolicy
appPolV v = \s -> v `index` enumState s

enumState :: BJState -> Finite 200
enumState s@BJState{..} =
  let offset = (dealerCard - 1) * 10 + playerSum - 12
   in if offset < 0
        then error (show s)
        else if usableAce
               then finite . fromIntegral $ 100 + offset
               else finite . fromIntegral $ offset

validStates :: [BJState] -> [BJState]
validStates =
  filter ( \ BJState{..} ->
                playerSum  >= 12 && playerSum  <= 21
             && dealerCard >= 1  && dealerCard <= 10
         )

data DbgT = DbgT
  { upCard :: Int
  , states :: [BJState]
  , reward :: Int
  }

play :: BJPolicy -> [Int] -> BJAction -> Writer [DbgT] (Int, [BJState])
play pol cards randAct =
  do let (r, ss) | pTot == 21 = if dTot == 21
                                  then (0, [])
                                  else (1, [])
                 | dTot == 21 = (-1, [])
                 | otherwise =
                   runWriter $ play' pol (drop 4 cards) dCards pCards Init randAct
     tell [ DbgT { upCard = cards !! 2
                 , states = ss
                 , reward = r
                 }
          ]
     return (r, ss)
  where (pTot, _) = cardSum pCards
        (dTot, _) = cardSum dCards
        dCards    = take 2 $ drop 2 cards
        pCards    = take 2 cards

data PlayPhase = Init    -- Bringing player's sum > 11.
               | Player  -- Player is hitting/standing.
               | Dealer  -- Dealer is hitting/standing.

play' :: BJPolicy   -- current policy
      -> [Int]      -- remaining deck of cards
      -> [Int]      -- dealer's cards
      -> [Int]      -- player's cards
      -> PlayPhase  -- phase of the game
      -> BJAction   -- randomly chosen action to be performed by player at his first free turn
      -> Writer [BJState] Int
play' pol cards dCards pCards phs randAct =
  case phs of
    Init ->
      if pTot > 11
        then do tell [s{didHit = act randAct}]
                if act randAct  -- Is first (randomly chosen) player action "hit"?
                  then play' pol (P.tail cards) dCards (P.head cards : pCards) Player randAct
                  else play' pol cards dCards pCards Dealer randAct
        else play' pol (P.tail cards) dCards (P.head cards : pCards) Init randAct

    Player ->
      do tell [s]
         if pTot > 21        -- Player go bust yet?
           then return (-1)
           else if polHit    -- Does policy dictate a hit?
                  then play' pol (P.tail cards) dCards (P.head cards : pCards) Player randAct
                  else play' pol cards dCards pCards Dealer randAct

    Dealer ->
      do tell [s]
         if dTot > 21        -- Dealer go bust yet?
           then return 1
           else if dTot < 17        -- Must dealer take a hit?
                  then play' pol (P.tail cards) (P.head cards : dCards) pCards Dealer randAct
                  else case compare pTot dTot of
                         GT -> return   1
                         EQ -> return   0
                         _  -> return (-1)

  where (pTot, ace) = cardSum pCards
        (dTot, _)   = cardSum dCards
        s'          = stateDef
                        { playerSum  = pTot
                        , usableAce  = ace
                        , dealerCard = P.last dCards
                        , dealerSum  = dTot
                        }
        s           = s'
                        { polHit = polHit
                        , didHit = polHit
                        }
        polHit      = (act . pol) s'

cardSum :: [Int] -> (Int, Bool)
cardSum cards =
  if tot < 12 && 1 `elem` cards
    then (tot + 10, True)
    else (tot,      False)
  where tot = sum cards

showVofState :: (Show a) => Vector 200 a -> String
showVofState v = intercalate "\n\n" $ map (showVofState' v) [True, False]

showVofState' :: Show a => Vector 200 a -> Bool -> String
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
                  [ v `index` enumState s
                  | dCard <- [1..10]
                  , let s = stateDef { playerSum  = pTot
                                     , usableAce  = ace
                                     , dealerCard = dCard
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

{----------------------------------------------------------------------
  Command line options defintions.
----------------------------------------------------------------------}

data Opts w = Opts
    { nGames :: w ::: Maybe Int <?>
        "The number of games to play."
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
    unwrapRecord "A solution to the 'Blackjack' problem (Example 5.3)."
  let nG = fromMaybe 10000 (nGames o)

  -- Initialize policy and state/action values.
  let qs  = VS.replicate ((0,0), (0,0))  -- Initialize all state/action values to zero.
      pol = VS.replicate (BJAction True)
        // [ (enumState s, BJAction False)
           | s <- [ stateDef { playerSum = pTot
                             , usableAce = ace
                             , dealerCard = dCard
                             }
                  | pTot  <- [20, 21]
                  , ace   <- [False, True]
                  , dCard <- [1..10]
                  ]
           ]

  -- Play many games, updating policy and state/action values after each.
  (results, _) <- runWriterT $ iterateM' nG optPol (qs, pol)
  writeFile  "other/blackjack.md" "\n### Final State Action Values (stand, hit)\n\n"
  appendFile "other/blackjack.md" $ pack
                                  $ showVofState
                                  $ VS.map ( toBoth
                                             $ PrettyFloat
                                             . uncurry safeDiv
                                             . (fromIntegral *** fromIntegral)
                                           )
                                  $ fst results
  appendFile "other/blackjack.md" "\n### Final Policy (H = hit)\n\n"
  appendFile "other/blackjack.md" $ pack
                                  $ showVofState
                                  $ snd results
  appendFile "other/blackjack.md" "\n### Debugging Info\n\n"
  appendFile "other/blackjack.md" "\n### State Action Visits (stand, hit)\n\n"
  appendFile "other/blackjack.md" $ pack
                                  $ showVofState
                                  $ VS.map ( toBoth snd )
                                  $ fst results

{----------------------------------------------------------------------
  Monte Carlo optimizer
----------------------------------------------------------------------}

-- | Play one game of Blackjack and return improved state-action value
-- and policy estimates.
--
-- Note that the Q vector contains not only the running expectation for
-- hitting/standing at each state, but also the number of times that
-- particular state-action pair has been visited. This is needed for
-- proper normalization across multiple episodes.
optPol :: ( Vector 200 ((Int, Integer), (Int, Integer))
          , Vector 200 BJAction
          )
       -> WriterT [[DbgT]]
            IO ( Vector 200 ((Int, Integer), (Int, Integer))
               , Vector 200 BJAction
               )
optPol (qs, pol) = do
  randAct <- lift $ rand allActs
  cards <- shuffleM $ concat [replicate  4  n | n <- [1..9]]
                           ++ replicate 16 10
  let ((r, ss), dbgs) = runWriter $ play (appPolV pol) cards randAct
      ixs             = (nubOn fst . map (enumState &&& didHit) . validStates) ss
      (qs', pol')     = VS.unzip $ VS.zip qs pol //
                          [ (i, (q', p'))
                          | (i, hit) <- ixs
                          , let q  = qs  `index` i
                                q' = ( if hit
                                         then second
                                         else first
                                     ) ((+ r) *** (+ 1)) q
                                p' = case uncurry compare
                                          $ toBoth ( uncurry (/)
                                                   . (fromIntegral *** fromIntegral)
                                                   ) q'
                                     of
                                       GT -> BJAction False
                                       _  -> BJAction True
                          ]
  tell [dbgs]
  return (qs', pol')

{----------------------------------------------------------------------
  Misc.
----------------------------------------------------------------------}

iterateM' :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM' 0 _ a = return a
iterateM' n f a = f a >>= iterateM' (n-1) f

toBoth :: (a -> b) -> (a,a) -> (b,b)
toBoth f (x,y) = (f x, f y)

safeDiv :: (Fractional a, Eq a)
        => a -> a -> a
safeDiv x y = if y == 0
                then 0
                else x / y

newtype PrettyFloat = PrettyFloat Float

instance Show PrettyFloat where
  show (PrettyFloat x) = printf "%+5.2f" x

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn _ []     = []
nubOn f (x:xs) = x : filter ((/= f x) . f) xs

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

