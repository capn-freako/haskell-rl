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
-- {-# OPTIONS_GHC -Wno-missing-signatures #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}
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
-- {-# LANGUAGE BangPatterns #-}
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

import Protolude  hiding (show, for)
import Options.Generic

import qualified Data.Vector.Sized as VS

import Control.Arrow                        ((&&&), (***))
import Control.Monad.Extra                  (skip)
import Control.Monad.Writer
import Data.Vector.Sized (Vector, (//), index)
import Data.Finite
import Data.List                            (nub)
-- import Data.Finite.Internal
-- import Data.List                            (findIndices)
import Data.Text                            (pack)
-- import Graphics.Rendering.Chart.Easy hiding (Wrapped, Unwrapped, Empty)
-- import Graphics.Rendering.Chart.Backend.Cairo
import System.Random.Shuffle
import Text.Printf

-- import RL.GPI
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
  } deriving (Show)

-- type BJAction = Bool  -- Hit?
newtype BJAction = BJAction { act :: Bool }  -- Hit?
instance Show BJAction where
  show (BJAction x) =
    if x then "H"
         else " "

type BJPolicy = BJState -> BJAction

appPolV :: Vector 200 BJAction -> BJPolicy
appPolV v = \s -> v `index` (enumState s)

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

play :: BJPolicy -> [Int] -> Writer [DbgT] (Int, [BJState])
play pol cards = do
  let (r, ss) =
        if pTot == 21 && dTot == 21  -- Checking for naturals.
          then (0, [initState])
          else if pTot == 21
                 then (1, [initState])
                 else if dTot == 21
                        then if pTot > 11
                               then (-1, [initState])
                               else (-1, [])
                        else runWriter $ play' pol (drop 4 cards) dCards pCards
  tell [ DbgT { upCard = cards !! 2
              , states = ss
              , reward = r
              }
       ]
  return (r, ss)
  where (pTot, ace) = cardSum pCards
        (dTot, _)   = cardSum dCards
        dCards      = (take 2 $ drop 2 cards)
        pCards      = (take 2 cards)
        initState'  = BJState pTot ace (P.head dCards) dTot False
        initState   = initState' { polHit = (act . pol) initState' }

play' :: BJPolicy -> [Int] -> [Int] -> [Int] -> Writer [BJState] Int
play' pol cards' dCards pCards =
  do if pTot > 11
       then tell [s]
       else skip
     if pTot > 21                         -- Player go bust yet?
       then return (-1)
       else if dTot > 21                  -- Dealer go bust yet?
              then return 1
              else if pTot < 12 || polHit -- Does player want a hit?
                     then play' pol (P.tail cards') dCards $ P.head cards' : pCards
                     else if dTot < 17  -- Must dealer take a hit?
                            then play' pol (P.tail cards') (P.head cards' : dCards) pCards
                            else case (compare pTot dTot) of
                                   GT -> return   1
                                   EQ -> return   0
                                   _  -> return (-1)
  where (pTot, ace) = cardSum pCards
        (dTot, _)   = cardSum dCards
        s'          = BJState pTot ace (P.last dCards) dTot False
        s           = s' { polHit = polHit }
        polHit      = (act . pol) s'

cardSum :: [Int] -> (Int, Bool)
cardSum cards =
  if tot < 12
    then if 1 `elem` cards
           then (tot + 10, True)
           else (tot,      False)
    else (tot, False)
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
                  [ v `index` (enumState s)
                  | dCard <- [1..10]
                  , let s = BJState pTot ace dCard 0 False
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
           | s <- [ BJState pTot ace dCard 0 False
                  | pTot  <- [20, 21]
                  , ace   <- [False, True]
                  , dCard <- [1..10]
                  ]
           ]

  -- Play many games, updating policy and state/action values after each.
  (results, dbgss) <- runWriterT $ iterateM' nG optPol (qs, pol)
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
  -- appendFile "other/blackjack.md" $ pack
  --                                 $ intercalate " " . map show
  --                                 $ map length . group . sort
  --                                 $ (map upCard . concat) dbgss
  -- -- print $ ((reward &&& states) . P.head . concat) dbgss
  -- -- mapIO (print . (reward &&& states)) $ (take 5 . concat) dbgss
  -- forM_ ((take 5 . concat) dbgss) (print . (reward &&& states))

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
  cards <- shuffleM $ concat [replicate  4  n | n <- [1..9]]
                           ++ replicate 16 10
  let ((r, ss), dbgs) = runWriter $ play (appPolV pol) cards
      ixs             = nub . map enumState . validStates $ ss
      (qs', pol')     = VS.unzip $ VS.zip qs pol //
                          [ (i, (q', p'))
                          | i <- ixs
                          , let q  = qs  `index` i
                                p  = pol `index` i
                                q' = ( if (act p)
                                         then second
                                         else first
                                     ) ((+ r) *** (+ 1)) q
                                -- p' = if fst q' > snd q'
                                p' = case ( uncurry compare
                                          $ toBoth ( uncurry (/)
                                                   . (fromIntegral *** fromIntegral)
                                                   ) q'
                                          ) of
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

