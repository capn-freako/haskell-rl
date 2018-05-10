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
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad.Writer
import qualified Data.Vector.Sized   as VS
import Data.Finite
import Data.Finite.Internal
import Data.Text                            (pack)
import Graphics.Rendering.Chart.Easy hiding (Wrapped, Unwrapped, Empty)
import Graphics.Rendering.Chart.Backend.Cairo

import RL.GPI
\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}
{----------------------------------------------------------------------
  Problem specific definitions
----------------------------------------------------------------------}

eps'   = 0.1  -- my choice
gamma' = 0.9  -- dictated by Exercise 4.7.

-- expectations for Poisson distributions
gEXPREQ1 = 3
gEXPREQ2 = 4
gEXPRET1 = 3
gEXPRET2 = 2

pReq1  = poisson' $ finite gEXPREQ1
pReq2  = poisson' $ finite gEXPREQ2
-- pRet1  = poisson' $ finite gEXPRET1
-- pRet2  = poisson' $ finite gEXPRET2

type RCState  = (Finite 21, Finite 21)  -- ^ # of cars at locations 1 and 2.
type RCAction = Int                     -- ^ # of cars to move from 1 to 2

-- | S
allStates :: [RCState]
allStates = [(finite m, finite n) | m <- [0..20], n <- [0..20]]

-- Just a sized vector alternative to the list above.
allStatesV :: VS.Vector 441 (Finite 21, Finite 21)
allStatesV = fromMaybe (VS.replicate (finite 0, finite 0))
                       $ VS.fromList allStates

-- | A(s)
actions' :: RCState -> [RCAction]
actions' (Finite n1, Finite n2) = map fromIntegral [-(min 5 n2) .. min 5 n1]

-- | S'(s, a)
nextStates' :: RCState -> RCAction -> [RCState]
nextStates' (Finite n1, Finite n2) a =
  [ (finite m1, finite m2)
  | m1 <- [max 0 (min 20 (n1 - a') - 11) .. min 20 (min 20 (n1 - a') + 11)]
  , m2 <- [max 0 (min 20 (n2 + a') - 11) .. min 20 (min 20 (n2 + a') + 11)]
  ]
 where a' = fromIntegral a

-- | R(s, a, s')
--
-- Returns a list of pairs, each containing:
-- - a unique reward value, and
-- - the probability of occurence for that value.
rewards' :: RCState -> RCAction -> RCState -> [(Float, Float)]
rewards' (Finite n1, Finite n2) a (Finite n1', Finite n2') =
  -- [ ( fromIntegral (10 * (nReq1' + nReq2') - 2 * abs a')
  [ ( fromIntegral (10 * (nReq1' + nReq2') - fromIntegral pnlty)
    , product $
        zipWith ($) [ pReq1, pReq2 ]
                    [ (finite . fromIntegral) nReq1
                    , (finite . fromIntegral) nReq2
                    ]
    )
  | nReq1 <- [0..11]
  , nReq2 <- [0..11]
  , let m1     = min 20 (n1 - a')   -- # on lot in the morning
        m2     = min 20 (n2 + a')
        nReq1' = min nReq1 m1       -- # actually rented
        nReq2' = min nReq2 m2
        pnlty  = ( if a > 0
                     then  2 * (a - 1)
                     else -2 * a
                 )
               + ( if m1 > 10
                     then 4
                     else 0
                 )
               + ( if m2 > 10
                     then 4
                     else 0
                 )
  -- Copying the same "cheat" used in the Python code.
  -- (i.e. - # of cars returned assumed to equal expectaion.)
  , if n1' == 20 then gEXPRET1 >= (n1' + nReq1' - m1)
                   && gEXPRET1 <= 11
                 else gEXPRET1 == n1' + nReq1' - m1
  , if n2' == 20 then gEXPRET2 >= (n2' + nReq2' - m2)
                   && gEXPRET2 <= 11
                 else gEXPRET2 == n2' + nReq2' - m2
  ]
 where a' = fromIntegral a

showFofState :: (Show a) => (RCState -> a) -> String
showFofState g = unlines
  ( "\\begin{array}{c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}" :
    ( ("\\text{cars at loc. 1} &" ++ intersperse '&' (replicate 21 ' ') ++ " \\\\") :
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
      ++ [intercalate " & " $ "\\text{cars at loc. 2:} " : [show n | n <- [0..20]]]
      ++ ["\\end{array}"]
    )
  )

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
    unwrapRecord "A solution to the 'Jack's Rental Cars' problem (Ex. 4.7)."
  let nIters = fromMaybe 2 (nIter o)
      nEvals = fromMaybe 1 (nEval o)

  -- Plot the pdfs.
  writeFile  "other/rentalcars.md"
             "### Return/Request Probability Distribution Functions\n\n"
  toFile def "img/pdfs.png" $
    do layout_title .= "Return/Request Probability Distribution Functions"
       setColors $ map opaque [red, blue, green, yellow]
       forM_ ( zip ["Req1", "Req2", "Ret1", "Ret2"]
                   [3,      4,      3,      2]
             ) $ \ (lbl, n) ->
                   plot ( line lbl
                               [ [ (x, poisson' (finite n) (finite x))
                                 | x <- [0..20]
                                 ]
                               ]
                        )
  appendFile "other/rentalcars.md" "![](img/pdfs.png)\n"

  -- Calculate and display optimum policy.
  appendFile "other/rentalcars.md" "\n### Policy optimization\n\n"
  let iters = take (nIters + 1)
                   -- $ iterate ( optPol gamma'  eps'       nEvals allStatesV
                   --                    actions nextStates rewards
                   $ iterate
                       ( optPol
                           rltDef
                             { gamma      = gamma'
                             , epsilon    = eps'
                             , maxIter    = nEvals
                             , states     = allStatesV
                             , actions    = actions'
                             , nextStates = nextStates'
                             , rewards    = rewards'
                             }
                       ) (const (0,0), "")
      acts  = map ((\f -> VS.map (fst . f) allStatesV) . fst) iters
      diffs = map (VS.map (fromIntegral . abs) . uncurry (-))
                  $ zip acts (P.tail acts)
  let ((_, (g', _)), msg) = first (fromMaybe (P.error "main: Major failure!")) $
        runWriter $ withinOnM eps'
                              ( \ (dv, (_, msg')) ->
                                  do tell msg'
                                     maxAndNonZero "\n**Found %3d policy changes.**\n\n"
                                                   dv
                              ) $ zip diffs (P.tail iters)
  appendFile "other/rentalcars.md" $ pack msg

  let pol   = fst . g'
      val   = snd . g'
  appendFile "other/rentalcars.md" "\n### Final policy\n\n"
  appendFile "other/rentalcars.md" $ pack $ showFofState pol
  appendFile "other/rentalcars.md" "\n### Final value function\n\n"
  appendFile "other/rentalcars.md" $ pack $ showFofState (Pfloat . val)
  -- appendFile "other/rentalcars.md" "\n### E[reward]\n\n"
  -- appendFile "other/rentalcars.md" $ pack $ showFofState (Pfloat . testRewards)

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

