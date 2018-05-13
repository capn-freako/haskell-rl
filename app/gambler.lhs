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
import Data.List                            (findIndices)
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
gamma' = 1     -- Dictated by problem.

type GState  = Int
type GAction = Int

allStatesV = VS.generate P.id :: VS.Vector 101 Int

actions' :: GState -> [GAction]
actions' s = [0 .. min s (100 - s)]

nextStates' :: GState -> GAction -> [GState]
nextStates' s a = if a == 0
                   then [s]
                   else [s - a, s + a]

rewards' :: Float -> GState -> GAction -> GState -> [(Float, Float)]
rewards' ph' s a s' = [(0, p)]
 where p = if s' == s + a
             then ph'
             else 1 - ph'

{----------------------------------------------------------------------
  Command line options defintions.
----------------------------------------------------------------------}

data Opts w = Opts
    { nIter :: w ::: Maybe Int <?>
        "The number of policy improvement iterations"
    , nEval :: w ::: Maybe Int <?>
        "The number of policy evaluation iterations per policy improvement iteration"
    , eps :: w ::: Maybe Float <?>
        "Convergence tolerance"
    , ph :: w ::: Maybe Float <?>
        "Probability of heads"
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
  let nIters = fromMaybe 10   (nIter o)
      nEvals = fromMaybe  1   (nEval o)
      eps'   = fromMaybe  0.1 (eps   o)
      ph'    = fromMaybe  0.4 (ph    o)

  -- Show policy/value convergence for different values of `nEval`.
  forM_ (zip [(1::Int)..4] [(0::Int),1,2,5]) $ \(n, nEvals') -> do
    let iters = take (nIters + 1)
                     $ iterate
                         ( optPol
                             rltDef
                               { gamma      = gamma'
                               , epsilon    = eps'
                               , maxIter    = nEvals'
                               , states     = allStatesV
                               , actions    = actions'
                               , nextStates = nextStates'
                               , rewards    = rewards' ph'
                               , stateVals  =
                                   [ (  0, 0)
                                   , (100, 1)
                                   ]
                               }
                         ) (\s -> if s == 100 then (0,1) else (0,0), [])
        acts  = map ((\f -> VS.map (fst . f) allStatesV) . fst) iters
        diffs = map (VS.map (fromIntegral . abs) . uncurry (-))
                    $ zip acts (P.tail acts)
        ((_, (_, _)), cnts) = first (fromMaybe (P.error "main: Major failure!")) $
          runWriter $ withinOnM eps'
                                ( \ (dv, (_, cnts')) ->
                                    do tell $ map negate cnts'  -- to distinguish them
                                       maxAndNonZero dv
                                ) $ zip diffs (P.tail iters)
    toFile def (printf "img/gam_cnv%d.png" n) $
      do layout_title .= (printf "Value Function & Policy Convergence (nEval = %d)" nEvals')
         layout_x_axis . laxis_title .= "Iteration (mixed)"
         layout_y_axis . laxis_title .= "# of diffs >eps."
         plot (line "Value"  [zip (findIndices (<= 0) cnts) (map negate $ filter (<= 0) cnts)])
         plot (line "Policy" [zip (findIndices (> 0) cnts)  (filter (> 0) cnts)])

  writeFile  "other/gambler.md" (pack $ printf "\n$eps = %04.2g$\n" eps')
  appendFile "other/gambler.md" "\n### Value/Policy convergence vs. `nEval`\n\n"
  appendFile "other/gambler.md" (pack $ printf "$ph = %04.2f$\n\n" ph')
  appendFile "other/gambler.md" "Note: `nEval` of zero is _Value Iteration_.\n\n"
  appendFile "other/gambler.md" "|     |     |\n"
  appendFile "other/gambler.md" "| --- | --- |\n"
  appendFile "other/gambler.md" "| ![](img/gam_cnv1.png) | ![](img/gam_cnv2.png) |\n"
  appendFile "other/gambler.md" "| ![](img/gam_cnv3.png) | ![](img/gam_cnv4.png) |\n"

  -- Plot the state value functions and final policies for different `ph` values.
  forM_ (zip [(1::Int)..4] [(0.25::Float),0.4,0.5,0.55]) $ \(n, ph'') -> do
    let iters' = take (nIters + 1)
                     $ iterate
                         ( optPol
                             rltDef
                               { gamma      = gamma'
                               , epsilon    = eps'
                               , maxIter    = nEvals
                               , states     = allStatesV
                               , actions    = actions'
                               , nextStates = nextStates'
                               , rewards    = rewards' ph''
                               , stateVals  =
                                   [ (  0, 0)
                                   , (100, 1)
                                   ]
                               }
                         ) (\s -> if s == 100 then (0,1) else (0,0), [])
        acts'  = map ((\f -> VS.map (fst . f) allStatesV) . fst) iters'
        diffs' = map (VS.map (fromIntegral . abs) . uncurry (-))
                    $ zip acts' (P.tail acts')
        ((_, (g'', _)), _) = first (fromMaybe (P.error "main: Major failure!")) $
          runWriter $ withinOnM eps'
                                ( \ (dv, (_, cnts')) ->
                                    do tell $ map negate cnts'  -- to distinguish them
                                       maxAndNonZero dv
                                ) $ zip diffs' (P.tail iters')
        pol'   = fst . g''
        val'   = snd . g''
        vs'    = map (\(g, _) -> snd . g) iters'
    toFile def (printf "img/gam_val%d.png" n) $
      do layout_title .= (printf "State Value Functions (ph = %04.2f)" ph'')
         layout_x_axis . laxis_title .= "Iteration (mixed)"
         layout_y_axis . laxis_title .= "# of diffs' >eps."
         setColors $ map opaque [red, green, blue, black]
         forM_ ( zip ["1 Iter.", "2 Iters.", "3 Iters."]
                     [1,      2,      3]
               ) $ \ (lbl, n') ->
                     plot ( line lbl
                                 [ [ (x, (vs' P.!! n') x)
                                   | x <- [(0::GState)..100]
                                   ]
                                 ]
                          )
         plot ( line "Final"
                     [ [ (x, val' x)
                       | x <- [0..100]
                       ]
                     ]
              )
    toFile def (printf "img/gam_pol%d.png" n) $
      do layout_title .= (printf "Final Policy Function (ph = %04.2f)" ph'')
         layout_x_axis . laxis_title .= "State"
         layout_y_axis . laxis_title .= "Action"
         setColors $ map opaque [blue, green, black]
         plot ( line "pi(s)"
                     [ [ (x, pol' x)
                       | x <- [0..100]
                       ]
                     ]
              )

  appendFile "other/gambler.md" "\n### State Value Functions vs. `ph`\n\n"
  appendFile "other/gambler.md" (pack $ printf "$nEval = %d$\n\n" nEvals)
  appendFile "other/gambler.md" "|     |     |\n"
  appendFile "other/gambler.md" "| --- | --- |\n"
  appendFile "other/gambler.md" "| ![](img/gam_val1.png) | ![](img/gam_val2.png) |\n"
  appendFile "other/gambler.md" "| ![](img/gam_val3.png) | ![](img/gam_val4.png) |\n"

  appendFile "other/gambler.md" "\n### Final Policy Function vs. `ph`\n\n"
  appendFile "other/gambler.md" (pack $ printf "$nEval = %d$\n\n" nEvals)
  appendFile "other/gambler.md" "|     |     |\n"
  appendFile "other/gambler.md" "| --- | --- |\n"
  appendFile "other/gambler.md" "| ![](img/gam_pol1.png) | ![](img/gam_pol2.png) |\n"
  appendFile "other/gambler.md" "| ![](img/gam_pol3.png) | ![](img/gam_pol4.png) |\n"

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

