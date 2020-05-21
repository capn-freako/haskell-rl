-- Demonstrating subtleties involved in memoizing multi-argument functions.
--
-- Original author: David Banas <David.Banas@target.com>
-- Original date:   May 9, 2018
--
-- Copyright (c) 2018 David Banas; all rights reserved World wide.

module Test where

import Data.MemoTrie

gamma = 0.9

evalPol p s's rs v = let actVal'' = actVal' s's rs v
               in \s -> actVal'' (s, p s)

evalPol' p s's rs v = \s -> actVal' s's rs v (s, p s)

actVal' s's rs = memo . uncurry . actVal s's rs
-- actVal' s's rs = uncurry . actVal s's rs

actVal :: (s -> a -> [s]) -> (s -> a -> s -> [(Float, Float)])
       -> (s -> Float) -> s -> a -> Float
actVal s's rs v s a =
 sum [ pt * gamma * u + rt
     | s' <- s's s a
     , let u = v s'
     , let (pt, rt) = foldl prSum (0,0)
                            [ (p, p * r)
                            | (r, p) <- rs s a s'
                            ]
     ]
prSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)

