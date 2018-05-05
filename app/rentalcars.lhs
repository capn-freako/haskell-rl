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
{-# OPTIONS_GHC -Wno-orphans #-}
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
-- {-# LANGUAGE BangPatterns #-}
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
import Prelude (unlines, Show(..), String)

import Protolude  hiding (show, for)
import Options.Generic

import qualified Data.Vector.Sized   as VS
import Data.Finite
import Data.Finite.Internal
import Data.MemoTrie
import Data.Text                            (pack)
import Graphics.Rendering.Chart.Easy hiding (Wrapped, Unwrapped, Empty)
import Graphics.Rendering.Chart.Backend.Cairo
import Text.Printf
\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}
{----------------------------------------------------------------------
  Orphans
----------------------------------------------------------------------}
instance (KnownNat n) => HasTrie (Finite n) where
  data Finite n :->: x     = FiniteTrie (VS.Vector n x)
  trie f                   = FiniteTrie (VS.generate (f . finite . fromIntegral))
  untrie (FiniteTrie v)    = VS.index v
  enumerate (FiniteTrie v) = map (first (finite . fromIntegral)) $ (VS.toList . VS.indexed) v

{----------------------------------------------------------------------
  General policy iterator
----------------------------------------------------------------------}

-- | Yields a single policy improvment iteration, given:
--   - gamma           - discount rate
--   - n               - max. policy evaluation iterations
--   - A(s)            - all possible actions in state s,
--   - S'(s, a)        - all possible next states, for a given state/action pair, and
--   - R(s, a, s')     - all possible rewards for a given state/action/next-state triple,
--                       along with their probabilities of occurence.
--
-- Returns a combined policy & value function.
--
-- Note: The following note doesn't make sense, now, but will soon.
--       (See TODO item below.)
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
optPol :: (Data.MemoTrie.HasTrie s, HasTrie a, Num a)  -- "Num a" is TEMPORARY; See TODO above.
       => Float                              -- ^ discount rate
       -> Int                                -- ^ max. # of evaluation iterations per improvement iteration
       -> (s -> [a])                         -- ^ A(s)
       -> (s -> a -> [s])                    -- ^ S'(s, a)
       -> (s -> a -> s -> [(Float, Float)])  -- ^ R(s, a, s')
       -> (s -> (a, Float))                  -- ^ initial (combined) policy & value function
       -> s -> (a, Float)
optPol gamma n as s's rs g = bestA
 where
  bestA     = maximumBy (compare `on` snd) . aVals v'
  aVals v = \s -> let actVal'' = actVal' v
                   in [ (a, actVal'' (s, a))
                      | a <- as s
                      ]
  actVal'   = memo . uncurry . actVal
  actVal v s a =
    sum [ pt * gamma * u + rt
        | s' <- s's s a
        , let u = v s'
        , let (pt, rt) = foldl prSum (0,0)
                               [ (p, p * r)
                               | (r, p) <- rs s a s'
                               ]
        ]
  prSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)
  v' = P.last $ take n $ iterate (evalPol (fst . g)) $ snd . g
  evalPol p v = let actVal'' = actVal' v
                 in \s -> actVal'' (s, p s)

{----------------------------------------------------------------------
  Problem specific definitions
----------------------------------------------------------------------}

eps'   = 0.1  -- my choice
gamma' = 0.9  -- dictated by Exercise 4.7.
pReq1  = poisson' 3
pReq2  = poisson' 4
pRet1  = pReq1
pRet2  = poisson' 2

type RCState  = (Finite 21, Finite 21)  -- ^ # of cars at locations 1 and 2.
type RCAction = Int                     -- ^ # of cars to move from 1 to 2

-- | S
allStates :: [RCState]
allStates = [(finite m, finite n) | m <- [0..20], n <- [0..20]]

allStatesV :: VS.Vector 441 (Finite 21, Finite 21)
allStatesV = fromMaybe (VS.replicate (finite 0, finite 0))
                       $ VS.fromList allStates

-- | A(s)
actions :: RCState -> [RCAction]
actions (Finite n1, Finite n2) = map fromIntegral [-(min 5 n2) .. min 5 n1]

-- | S'(s, a)
nextStates :: RCState -> RCAction -> [RCState]
-- nextStates _ _ = allStates
nextStates (Finite n1, Finite n2) a =
  [ (finite m1, finite m2)
  | m1 <- [max 0 (n1 - a' - 11) .. min 20 (n1 - a' + 11)]
  , m2 <- [max 0 (n2 + a' - 11) .. min 20 (n2 + a' + 11)]
  ]
 where a' = fromIntegral a

-- | R(s, a, s')
--
-- Returns a list of pairs, each containing:
-- - a unique reward value, and
-- - the probability of occurence for that value.
rewards :: RCState -> RCAction -> RCState -> [(Float, Float)]
rewards (Finite n1, Finite n2) a (Finite n1', Finite n2') =
  [ ( fromIntegral (10 * (nReq1 + nReq2) - 2 * abs a)
    , product $
        zipWith ($) [ pReq1, pRet1, pReq2, pRet2]
                    [ (finite . fromIntegral) nReq1
                    , (finite . fromIntegral) nRet1
                    , (finite . fromIntegral) nReq2
                    , (finite . fromIntegral) nRet2
                    ]
    )
  -- n1 + nRet1 - a <= 20  ==>  nRet1 <= 20 + a - n1
  -- n2 + nRet2 + a <= 20  ==>  nRet2 <= 20 - a - n2
  | nRet1 <- [max 0 (n1' + a' - n1) .. min 11 (20 + a' - n1)]
  , nRet2 <- [max 0 (n2' - a' - n2) .. min 11 (20 - a' - n2)]
  , let nReq1 = fromIntegral (n1 + nRet1 - a' - n1')  -- >= 0 => nRet1 >= n1' + a - n1
                                                      -- <= 11 => nRet1 <= 11 + a + n1' - n1
        nReq2 = fromIntegral (n2 + nRet2 + a' - n2')  -- >= 0 => nRet2 >= n2' - a - n2
  -- , nRet1 < 21 && nRet2 < 21 && nReq1 < 21 && nReq2 < 21
  , nReq1 <= 11 && nReq2 <= 11
  ]
 where a' = fromIntegral a
{----------------------------------------------------------------------
  Command line options defintions.
----------------------------------------------------------------------}

data Opts w = Opts
    { nIter :: w ::: Maybe Int <?> "The number of policy improvement iterations"
    , nEval :: w ::: Maybe Int <?> "The number of policy evaluation iterations per policy improvement iteration"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)


{----------------------------------------------------------------------
  main()
----------------------------------------------------------------------}

main :: IO ()
main = do
  -- Process command line options.
  o :: Opts Unwrapped <- unwrapRecord "A solution to the 'Jack's Rental Cars' problem (Ex. 4.7)."
  let nIters = fromMaybe 2 (nIter o)
      nEvals = fromMaybe 1 (nEval o)

  -- Plot the pdfs.
  writeFile  "other/rentalcars.md" "### Return/Request Probability Distribution Functions\n\n"
  toFile def "img/pdfs.png" $
    do layout_title .= "Return/Request Probability Distribution Functions"
       setColors $ map opaque [red, blue, green, yellow]
       forM_ ( zip ["Req1", "Req2", "Ret1", "Ret2"]
                   [3,      4,      3,      2]
             ) $ \ (lbl, n) -> plot (line lbl [[(x, poisson' (finite n) (finite x)) | x <- [0..20]]])
  appendFile "other/rentalcars.md" "![](img/pdfs.png)\n"

  -- Calculate and display optimum policy.
  -- let g = P.last $ take nIters $ iterate (optPol gamma' nEvals actions nextStates rewards) (const (0,0))
  let g     = take nIters
                   $ iterate (optPol gamma' nEvals actions nextStates rewards)
                             (const (0,0))
      acts  = map (\f -> VS.map (fst . f) allStatesV) g
      diffs = map (VS.maximum . VS.map (fromIntegral . abs) . uncurry (-))
                  $ zip acts (P.tail acts)
      g'    = snd . fromMaybe (0, const (0,0)) $ withinOn eps' fst $ zip diffs (P.tail g)
      pol   = fst . g'
      val   = snd . g'
  appendFile "other/rentalcars.md" "\n### Final policy\n\n"
  appendFile "other/rentalcars.md" $ pack $ showFofState pol
  appendFile "other/rentalcars.md" "\n### Final value function\n\n"
  appendFile "other/rentalcars.md" $ pack $ showFofState (Pfloat . val)

{----------------------------------------------------------------------
  Misc.
----------------------------------------------------------------------}

--- | To control the formatting of printed floats in output matrices.
newtype Pfloat = Pfloat { unPfloat :: Float}
  deriving (Eq)

instance Show Pfloat where
  show x = printf "%4.1f" (unPfloat x)

showFofState :: (Show a) => (RCState -> a) -> String
showFofState g = unlines
  ( "\\begin{array}{c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}" :
    ( ("\\text{cars at loc. 1} &" ++ (intersperse '&' (replicate 21 ' ')) ++ " \\\\") :
      ["\\hline"] ++
      intersperse "\\hline"
        ( map ((++ " \\\\") . intercalate " & ")
              [ (show m :) $ map show
                [ g (finite m, finite n)
                | n <- [0..20]
                ]
              | m <- [0..20]
              ]
        )
      ++ ["\\hline"]
      ++ [(intercalate " & " $ "\\text{cars at loc. 2:} " : [show n | n <- [0..20]])]
      ++ ["\\end{array}"]
    )
  )

poisson :: Int -> Int -> Float
poisson lambda n = lambda' ^ n' * exp (-lambda') / fromIntegral (fact n)
 where lambda' = fromIntegral lambda
       n'      = fromIntegral n

fact :: Int -> Int
fact 0 = 1
fact n = product [1..n]

poissonVals :: VS.Vector 5 (VS.Vector 12 Float)
poissonVals = VS.generate (VS.generate . poisson)

poisson' :: Finite 5 -> Finite 21 -> Float
poisson' n x@(Finite x') =
  if x > 11  -- The Python code enforces this limit. And we're trying
    then 0   -- for an "apples-to-apples" performance comparison.
    else poissonVals `VS.index` n `VS.index` (finite x')

-- TODO: Eliminate the double work going on here.
withinOn :: Float -> (a -> Float) -> [a] -> Maybe a
withinOn _   _ []              = Nothing
withinOn _   _ (x : [])        = Just x
withinOn eps f (x : xs@(y:_)) =
  if abs (f x - f y) < eps
    then Just y
    else withinOn eps f xs

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

