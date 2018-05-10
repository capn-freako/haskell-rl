```include
other/header.md
```

haskell-rl : Gambler's Problem (Ex. 4.9)
===

This [literate Haskell](https://wiki.haskell.org/Literate_programming) document provides a solution to Exercise 4.9 in _Reinforcement Learning_ by Sutton & Barto.

Original author: [David Banas](mailto:David.Banas@target.com)  
Original date:   May 10, 2018

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
- [Chart](https://www.stackage.org/package/Chart)
- [Chart-cairo](https://www.stackage.org/package/Chart-cairo)

\begin{code}
import qualified Prelude as P
-- import Prelude (unlines, Show(..), String)

import Protolude  hiding (show, for)
import Options.Generic

import Control.Monad.Writer
import qualified Data.Vector.Sized   as VS
-- import Data.Finite
-- import Data.Finite.Internal
import Data.Text                            (pack)
import Graphics.Rendering.Chart.Easy hiding (Wrapped, Unwrapped, Empty)
import Graphics.Rendering.Chart.Backend.Cairo
import Text.Printf

import RL.GPI
\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}
{----------------------------------------------------------------------
  Problem specific definitions
----------------------------------------------------------------------}
eps'   = 0.1   -- My choice.
gamma' = 1     -- Dictated by problem.
-- ph     = 0.25
ph     = 0.4

type GState  = Int
type GAction = Int

allStatesV = VS.generate P.id :: VS.Vector 101 Int

actions :: GState -> [GAction]
actions s = [0 .. min s (100 - s)]

nextStates :: GState -> GAction -> [GState]
nextStates s a = if a == 0
                   then [s]
                   else [s - a, s + a]

rewards :: GState -> GAction -> GState -> [(Float, Float)]
rewards s a s' = if s' == 100 && s /= 100
                   then [(1, ph)]
                   else [(0, p)]
 where p = if s' == s + a
             then ph
             else 1 - ph

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
    unwrapRecord "A solution to the 'Gambler's Problem' (Ex. 4.9)."
  let nIters = fromMaybe 10 (nIter o)
      nEvals = fromMaybe  1 (nEval o)

  -- Calculate and display optimum policy.
  writeFile "other/gambler.md" "\n### Policy optimization\n\n"
  let iters = take (nIters + 1)
                   $ iterate ( optPol gamma'  eps'       nEvals allStatesV
                                      actions nextStates rewards
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
  appendFile "other/gambler.md" $ pack msg

  let pol   = fst . g'
      val   = snd . g'
      vs    = map (\(g, _) -> snd . g) iters

  -- Plot the state value functions.
  appendFile  "other/gambler.md"
             "### State Value Functions\n\n"
  toFile def "img/gam_val.png" $
    do layout_title .= "State Value Functions"
       setColors $ map opaque [red, blue, green, black]
       forM_ ( zip ["1 Iter.", "2 Iters.", "3 Iters."]
                   [1,      2,      3]
             ) $ \ (lbl, n) ->
                   plot ( line lbl
                               [ [ (x, (vs P.!! n) x)
                                 | x <- [(0::GState)..100]
                                 ]
                               ]
                        )
       plot ( line (printf "%d Iters." nIters)
                   -- [ [ (x, val x)
                   [ [ (x, (P.last vs) x)
                     | x <- [0..100]
                     ]
                   ]
            )
  appendFile "other/gambler.md" "![](img/gam_val.png)\n"

  -- Plot the final policy.
  appendFile  "other/gambler.md"
             "\n### Final Policy Function\n\n"
  toFile def "img/gam_pol.png" $
    do layout_title .= "Final Policy Function"
       setColors $ map opaque [blue, green, black]
       plot ( line "pi(s)"
                   [ [ (x, pol x)
                     | x <- [0..100]
                     ]
                   ]
            )
  appendFile "other/gambler.md" "![](img/gam_pol.png)\n"

\end{code}

output
---

```include
other/gambler.md
```

***

<div class="footer">
Powered by [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/) and [pandoc](http://pandoc.org/).
</div>

