```include
other/header.md
```

haskell-rl : Rental Car Problem (Ex. 4.7 from Sutton & Bartow)
===

This [literate Haskell](https://wiki.haskell.org/Literate_programming) document provides a solution to Exercise 4.7 in _Reinforcement Learning_ by Sutton & Barotw.

Original author: [David Banas](mailto:David.Banas@target.com)  
Original date:   April 20, 2018

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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
\end{code}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

\begin{code}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
\end{code}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)
- [vector-sized](https://www.stackage.org/package/vector-sized)
- [finite-typelits](https://www.stackage.org/package/finite-typelits)
- [extra](https://www.stackage.org/package/extra)
- [finite-typelits](https://www.stackage.org/package/finite-typelits)
- [text](https://www.stackage.org/package/text)
- [random](https://www.stackage.org/package/random)
- [random-shuffle](https://www.stackage.org/package/random-shuffle)
- [Chart](https://www.stackage.org/package/Chart)
- [Chart-cairo](https://www.stackage.org/package/Chart-cairo)
- [MemoTrie](https://hackage.haskell.org/package/MemoTrie)

\begin{code}
import qualified Prelude as P
-- import Prelude (tail, last, unlines, String, Show(..))
import Prelude (unlines, Show(..), String)

import Protolude  hiding (show, for)
import Options.Generic

import qualified Data.Vector.Sized   as VS
-- import qualified Data.Vector.Unboxed as V

import Control.Arrow              ((&&&), (***))
-- import Control.Monad.Extra        (unfoldM)
import Data.Finite
import Data.List                  (groupBy)
import Data.MemoTrie              (memo)
import Data.Text                  (pack)
-- import System.Random              (randomIO)
-- import System.Random.Shuffle      (shuffleM)
import Text.Printf                (printf)
-- import Test.QuickCheck            (generate, elements)
import Graphics.Rendering.Chart.Easy hiding (Wrapped, Empty)
import Graphics.Rendering.Chart.Backend.Cairo
\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}
{----------------------------------------------------------------------
  Command line options defintions.
----------------------------------------------------------------------}

data Opts w = Opts
    {
    -- { number :: w ::: Maybe Integer <?> "The number of games to play"
    -- , rate   :: w ::: Maybe Double  <?> "The learning rate"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)


{----------------------------------------------------------------------
  General policy iterator
----------------------------------------------------------------------}

-- | An optimum policy, given:
--   - A(s)            - all possible actions in state s,
--   - S'(s, a)        - all possible next states, for a given state/action pair,
--   - R(s, a, s')     - all possible rewards for a given state/action/next-state triple,
--                       along with their probabilities of occurence, and
--   - S               - all possible states.
--
-- Note: I use the pure random number generator, instead of its IO
--       counterpart, for two reasons:
--       - It allows me to keep this function pure, as opposed to
--         running it inside the IO monad.
--       - Initialized with a constant '1', the way it is here, it
--         provides the same stream of pseudo-random numbers each time
--         the program is run. This repeatability is a nice feature,
--         during program development/debug.
--       (Note, however, that it should be replaced by its IO
--       counterpart, once the code is stable, so as to provide better
--       corner case exposure.)
--
-- TODO: Make second argument to optPol' general, perhaps by choosing
--       randomly from among the legal actions for a given state.
optPol :: (KnownNat n, Num a)  -- "Num a" is TEMPORARY; See TODO above.
       => Integer                          -- ^ max. iterations
       -> Float                            -- ^ convergence tolerance
       -> Float                            -- ^ discount rate
       -> (s -> Finite n)                  -- ^ function from state to vector index
       -> (s -> [a])                       -- ^ A(s)
       -> (s -> a -> [s])                  -- ^ S'(s, a)
       -> (s -> a -> s -> [(Int, Float)])  -- ^ R(s, a, s')
       -> [s]                              -- ^ S
       -> ([s -> a], [[(VS.Vector n Float, Float)]])
optPol = optPol' [[(pure 0, 0)]] [const 0] 0

optPol' :: forall s a n. (KnownNat n)
        => [[(VS.Vector n Float, Float)]]   -- ^ results from previous iterations
        -> [s -> a]                         -- ^ results from previous iterations
        -> Integer                          -- ^ iterations so far
        -> Integer
        -> Float
        -> Float
        -> (s -> Finite n)
        -> (s -> [a])
        -> (s -> a -> [s])
        -> (s -> a -> s -> [(Int, Float)])
        -> [s]
        -> ([s -> a], [[(VS.Vector n Float, Float)]])
optPol' vs pols nIter maxIter eps gamma sToI acts s's rs ss =
  if nIter >= maxIter || stable
    then (reverse pols, reverse vs)
    else optPol' (v's : vs) (pol' : pols) (nIter + 1) maxIter eps gamma sToI acts s's rs ss
 where stable  = none ( uncurry (<)
                        . ( (uncurry (actVal gamma sToI s's rs v) . (P.id &&& P.head pols))
                            &&& (snd . bestA)
                          )
                      ) ss
       v    = (fst . P.last . P.head) vs
       v's  = evalPol eps gamma 2 0 pol' sToI s's rs ss (P.head vs)
       pol' = fst . bestA
       bestA :: s -> (a, Float)
       bestA s = maximumBy (compare `on` snd)
                   [ (a, actVal gamma sToI s's rs v s a)
                   | a <- acts s
                   ]

-- | Policy evaluator
--
-- Returns a list of better and better approximations to the value
-- function for a given policy, along with their deltas.
evalPol :: KnownNat n
        => Float                                -- ^ tolerance of convergence
        -> Float                                -- ^ discount rate
        -> Integer                              -- ^ max. iterations
        -> Integer                              -- ^ iterations so far
        -> (s -> a)                             -- ^ policy to be evaluated
        -> (s -> Finite n)                      -- ^ function from state to vector index
        -> (s -> a -> [s])                      -- ^ S'(s, a)
        -> (s -> a -> s -> [(Int, Float)])      -- ^ R(s, a, s')
        -> [s]                                  -- ^ S (all possible states)
        -> [(VS.Vector n Float, Float)]         -- ^ results of previous iterations
        -> [(VS.Vector n Float, Float)]
evalPol eps gamma maxIter nIter pol sToI s's rs ss vs =
  if nIter >= maxIter || delta < eps
    then reverse ((v', delta) : vs)
    else evalPol eps gamma maxIter (nIter + 1) pol sToI s's rs ss ((v', delta) : vs)
 where delta = maximum $ map ( abs
                             . uncurry (-)
                             . (appVal sToI v &&& appVal sToI v')
                             ) ss
       v'    = fromMaybe (VS.replicate 0) $ VS.fromList $ map (\s -> actVal gamma sToI s's rs v s (pol s)) ss
       v     = (fst . P.head) vs

-- | Apply the vector representation of a value function to a particular state.
appVal :: KnownNat n
       => (s -> Finite n)    -- ^ function from state to vector index
       -> VS.Vector n Float  -- ^ vector representation of value function
       -> s                  -- ^ state
       -> Float
appVal f v s = v `VS.index` (f s)

-- | Action-value function.
actVal :: KnownNat n
       => Float                                -- ^ discount rate
       -> (s -> Finite n)                      -- ^ function from state to vector index
       -> (s -> a -> [s])                      -- ^ S'(s, a)
       -> (s -> a -> s -> [(Int, Float)])      -- ^ R(s, a, s')
       -> VS.Vector n Float                    -- ^ vector representation of value function
       -> s                                    -- ^ initial state
       -> a                                    -- ^ next action
       -> Float
actVal gamma sToI s's rs v s a =
  sum [ p * (fromIntegral r + gamma * appVal sToI v s')
      | s'     <- s's s a
      , (r, p) <- rs s a s'
      ]

{----------------------------------------------------------------------
  Problem specific definitions
----------------------------------------------------------------------}

eps' = 0.1  -- my choice
gamma' = 0.9  -- dictated by Exercise 4.7.

newtype RCState  = RCState (Int, Int)  -- ^ # of cars at locations 1 & 2
  deriving (Show, Eq, Ord)

type    RCAction = Int                 -- ^ # of cars to move from 1 to 2
type    Policy   = RCState -> RCAction

instance Show Policy where
  show p = unlines $
    -- [ printf ((concat . replicate 21) "%2d  ") (map p [RCState (m,n) | n <- [0..20]])
    [ P.unwords . map (show . p) $ [RCState (m,n) | n <- [0..20]]
    | m <- [0..20]
    ]

-- | S
allStates :: [RCState]
allStates = [RCState (m,n) | m <- [0..20], n <- [0..20]]

stateToIndex :: RCState -> Finite 441
stateToIndex (RCState (n1, n2)) = (finite . fromIntegral) $ n1 * 21 + n2

pReq1  = poisson 3
pReq1' = memo pReq1
pReq2  = poisson 4
pReq2' = memo pReq2
pRet1  = poisson 3
pRet1' = memo pRet1
pRet2  = poisson 2
pRet2' = memo pRet2

-- | A(s)
asOfS :: RCState -> [RCAction]
asOfS (RCState s) = [-(min 5 (snd s)) .. min 5 (fst s)]

-- | S'(s, a)
nextStates :: RCState -> RCAction -> [RCState]
nextStates _ _ = allStates

-- | R(s, a, s')
--
-- Returns a list of pairs, each containing:
-- - a unique reward value, and
-- - the probability of occurence for that value.
rewards :: RCState -> RCAction -> RCState -> [(Int, Float)]
rewards (RCState (n1, n2)) a (RCState (n1', n2')) =
  map ((P.head *** sum) . unzip)
  . groupBy ((==) `on` fst)
  . sortOn fst $
  [ ( 10 * (nReq1 + nReq2) - 2 * abs(a)
    , sum [ product $ zipWith ($) [pReq1', pRet1', pReq2', pRet2']
                                  [nReq1,  nRet1,  nReq2,  nRet2]
          ]
    )
  | nRet1 <- [max 0 (n1'+a-n1) .. 20-n1]
  , nRet2 <- [max 0 (n2'-a-n2) .. 20-n2]
  , let nReq1 = n1 + nRet1 - a - n1'  -- >= 0 => nRet1 >= n1' + a - n1
        nReq2 = n2 + nRet2 + a - n2'  -- >= 0 => nRet2 >= n2' - a - n2
  ]

{----------------------------------------------------------------------
  main()
----------------------------------------------------------------------}

main :: IO ()
main = do
  -- Process command line options.
  -- o :: Opts Unwrapped <- unwrapRecord "A simple Tic-Tac-Toe example using reinforcement learning."
  -- let n   = fromMaybe 10  (number o)
  --     r   = fromMaybe 0.1 (rate   o)

  -- Plot the pdfs.
  writeFile  "other/rentalcars.md" "### Return/Request Probability Distribution Functions\n\n"
  toFile def "img/pdfs.png" $
    do layout_title .= "Return/Request Probability Distribution Functions"
       setColors $ map opaque [red, blue, green, yellow]
       forM_ ( zip ["Req1", "Req2", "Ret1", "Ret2"]
                   [3,      4,      3,      2]
             ) $ \ (lbl, n) -> plot (line lbl [[(x, poisson n x) | x <- [0..20]]])
  appendFile "other/rentalcars.md" "![](img/pdfs.png)\n"

  -- Calculate and display optimum policy.
  -- print $ optPol pdfGen eps' gamma' asOfS allStates
  -- appendFile "other/rentalcars.md" "\n### First 3 policies\n\n"
  -- forM_ (take 3 $ optPol pdfGen eps' gamma' asOfS allStates) $ \p ->
  --       appendFile "other/rentalcars.md" $ pack $ showPol p
  -- forM_ (take 3 $ optPol 2 pdfGen eps' gamma' asOfS allStates)
  --       $ appendFile "other/rentalcars.md" . pack . showPol
  let (pols, vss) = optPol 1 eps' gamma' stateToIndex asOfS nextStates rewards allStates
  appendFile "other/rentalcars.md" "\n### Second policy\n\n"
  -- appendFile "other/rentalcars.md" . pack . showPol $ P.head pols
  -- appendFile "other/rentalcars.md" . pack . showPol $ pols P.!! 1
  appendFile "other/rentalcars.md" "\n### Second policy's initial value function\n\n"
  -- forM_ (vss P.!! 1) (appendFile "other/rentalcars.md" . pack . showVal)
  appendFile "other/rentalcars.md" . pack . showVal $ vss P.!! 1 P.!! 0

{----------------------------------------------------------------------
  Misc.
----------------------------------------------------------------------}

showPol :: Policy -> String
showPol pol = showFofState pol

-- showVal :: ((RCState -> Float), Float) -> String
showVal :: (VS.Vector 441 Float, Float) -> String
showVal (v, delta) = showFofState (appVal stateToIndex v) ++ (printf "$\\quad \\delta = %5.3f$" delta)

showFofState :: (Show a) => (RCState -> a) -> String
showFofState g = unlines
  ( "\\begin{array}{c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c}" :
    intersperse "\\hline"
      ( map ((++ " \\\\") . intercalate " & " . map show)
            [ [g (RCState (m,n)) | n <- [0..20]] | m <- [0..20]]
      )
    ++ ["\\end{array}"]
  )

-- toBoth :: (a -> b) -> (a,a) -> (b,b)
-- toBoth f (x1, x2) = (f x1, f x2)

-- for :: (Functor f) => f a -> (a -> b) -> f b
-- for = flip map

poisson :: Int -> Int -> Float
-- poisson lambda n = pow lambda n * exp (-lambda) / fact n
poisson lambda n = lambda' ^ n' * exp (-lambda') / (fromIntegral $ fact n)
 where lambda' = fromIntegral lambda
       n'      = fromIntegral n

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

poissonVals :: VS.Vector 5 (VS.Vector 21 Float)
poissonVals = VS.generate $
                (\n -> VS.generate (poisson n . fromIntegral . getFinite))
                . fromIntegral . getFinite

poisson' :: Finite 5 -> Finite 21 -> Float
poisson' n x = poissonVals `VS.index` n `VS.index` x
\end{code}

output
---

```include
other/rentalcars.md
```

***

<div class="footer">
Powered by [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/) and [pandoc](http://pandoc.org/).
</div>

