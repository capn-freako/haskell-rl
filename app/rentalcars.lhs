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

\begin{code}
import qualified Prelude as P
-- import Prelude (tail, last, unlines, String, Show(..))
import Prelude (unlines, Show(..), String)

import Protolude  hiding (show, for)
import Options.Generic

-- import qualified Data.Vector.Sized   as VS
-- import qualified Data.Vector.Unboxed as V

import Control.Arrow              ((&&&), (***))
-- import Control.Monad.Extra        (unfoldM)
-- import Data.Finite
import Data.List                  (groupBy)
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
--   - p(s', r | s, a) - pdf of (next_state, reward),
--                       given (current_state, action),
--   - S'(s, a)        - all possible next states, for a given state/action pair,
--   - R(s, a, s')     - all possible rewards for a given state/action/next-state triple,
--   - A(s)            - all actions possible in state s, and
--   - S               - all possible states.
--
-- Note: In the code, the first 3 elements above are combined into a
--       single input function (first argument, below), which combines
--       the pdf with the 2 needed generators (i.e. - one for next states,
--       and another for rewards).
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
optPol :: Num a  -- TEMPORARY; See TODO above.
       => Integer                              -- ^ max. iterations
       -> (s -> a -> [(s, [(Float, Float)])])  -- ^ pdf (filtered for non-zero probs.)
       -> Float                                -- ^ convergence tolerance
       -> Float                                -- ^ discount rate
       -> (s -> [a])                           -- ^ A(s)
       -> [s]                                  -- ^ S
       -> ([s -> a], [[((s -> Float), Float)]])
optPol = optPol' [[((const 0), 0)]] [const 0] 0

optPol' :: forall s a.
           [[((s -> Float), Float)]]          -- ^ previous evalPol results
        -> [s -> a]                         -- ^ results from previous iterations
        -> Integer                          -- ^ iterations so far
        -> Integer
        -> (s -> a -> [(s, [(Float, Float)])])
        -> Float
        -> Float
        -> (s -> [a])
        -> [s]
        -> ([s -> a], [[((s -> Float), Float)]])
optPol' vs pols nIter maxIter gen eps gamma asofs ss =
  if nIter >= maxIter || stable
    then (reverse pols, reverse vs)
    -- else optPol' v' (pol' : pols) (nIter + 1) maxIter gen eps gamma asofs ss
    else optPol' (vofss : vs) (pol' : pols) (nIter + 1) maxIter gen eps gamma asofs ss
 where stable  = none ( uncurry (<)
                        . ( (uncurry (actVal gamma gen v) . (P.id &&& P.head pols))
                            &&& (snd . bestA)
                          )
                      ) ss
       v       = (fst . P.last . P.head) vs
       vofss   = evalPol eps gamma 10 0 gen pol' ss (P.head vs)
       pol'    = fst . bestA
       bestA :: s -> (a, Float)
       bestA s = maximumBy (compare `on` snd)
                   [ (a, actVal gamma gen v s a)
                   | a <- asofs s
                   ]

-- | Policy evaluator
--
-- Returns a list of better and better approximations to the value
-- function for a given policy, along with their deltas.
evalPol :: Float                                -- ^ tolerance of convergence
        -> Float                                -- ^ discount rate
        -> Integer                              -- ^ max. iterations
        -> Integer                              -- ^ iterations so far
        -> (s -> a -> [(s, [(Float, Float)])])  -- ^ pdf (filtered for non-zero probs.)
        -> (s -> a)                             -- ^ policy to be evaluated
        -> [s]                                  -- ^ S (all possible states)
        -> [((s -> Float), Float)]              -- ^ results of previous iterations
        -> [((s -> Float), Float)]
evalPol eps gamma maxIter nIter gen pol ss vofss =
  if nIter >= maxIter || delta < eps
    then reverse ((vofs', delta) : vofss)
    else evalPol eps gamma maxIter (nIter + 1) gen pol ss ((vofs', delta) : vofss)
 where delta   = maximum $ map (abs . uncurry (-) . (vofs &&& vofs')) ss
       vofs' s = actVal gamma gen vofs s (pol s)
       vofs    = (fst . P.head) vofss

-- | Action-value function.
--
-- Note: See the documentation for `optPol` for an explanation of the
--       second argument.
actVal :: Float                                -- ^ discount rate
       -> (s -> a -> [(s, [(Float, Float)])])  -- ^ pdf (filtered for non-zero probs.)
       -> (s -> Float)                         -- ^ value function
       -> s                                    -- ^ initial state
       -> a                                    -- ^ next action
       -> Float
actVal gamma gen vofs s a =
  sum [ p * (r + gamma * vofs s')
      | (s', rs) <- gen s a
      , (r, p)   <- rs
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

allStates :: [RCState]
allStates = [RCState (m,n) | m <- [0..20], n <- [0..20]]

pReq1 = poisson 3
pReq2 = poisson 4
pRet1 = poisson 3
pRet2 = poisson 2

-- | PDF generator, for original problem (i.e. - Example 4.2).
pdfGen :: RCState   -- ^ current state
       -> RCAction  -- ^ next action
       -> [(RCState, [(Float, Float)])]
pdfGen (RCState s) a =
  map ((P.head *** map (first fromIntegral)) . unzip)
  . groupBy ((==) `on` fst)
  . sortOn fst
  $ [ ( RCState (( (+ (nRet1 - a - nReq1)) *** (+ (nRet2 + a - nReq2)) ) s)
      , ( 10 * (nReq1 + nReq2) - 2 * abs(a)
        , product $
            zipWith ($) [pReq1, pRet1, pReq2, pRet2]
                        [nReq1, nRet1, nReq2, nRet2]
        )
      )
    | nRet1 <- [0..20-n1]
    , nRet2 <- [0..20-n2]
    , nReq1 <- [0..n1+nRet1-a]
    , nReq2 <- [0..n2+nRet2+a]
    ]
 where n1 = fst s
       n2 = snd s

-- | A(s)
asOfS :: RCState -> [RCAction]
asOfS (RCState s) = [-(min 5 (snd s)) .. min 5 (fst s)]

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
  let (pols, vss) = optPol 1 pdfGen eps' gamma' asOfS allStates  -- :: ([s -> a], [[((s -> Float), Float)]])
  appendFile "other/rentalcars.md" "\n### Second policy\n\n"
  -- appendFile "other/rentalcars.md" . pack . showPol $ P.head pols
  appendFile "other/rentalcars.md" . pack . showPol $ pols P.!! 1
  appendFile "other/rentalcars.md" "\n### Second policy's value function\n\n"
  -- forM_ (vss P.!! 1) (appendFile "other/rentalcars.md" . pack . showVal)
  (appendFile "other/rentalcars.md" . pack . showVal) (P.last $ vss P.!! 1)

{----------------------------------------------------------------------
  Misc.
----------------------------------------------------------------------}

showPol :: Policy -> String
showPol pol = showFofState pol

showVal :: ((RCState -> Float), Float) -> String
showVal (v, delta) = showFofState v ++ (printf "$\\quad \\delta = %5.3f$" delta)

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

