-----------------------------------------------------------------------------
-- |
-- Module      :  RL.GPI
-- Copyright   :  (c) Target Corp., 2018
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David.Banas@target.com
-- Stability   :  experimental
-- Portability :  ?
--
-- A general policy iterator for reinforcement learning.
--
-- Developed while doing the exercises in Ch. 4 of the book
-- /Reinforcement Learning: an Introduction/,
--   Richard S. Sutton and Andrew G. Barto
--     The MIT Press
--       Cambridge, Massachusetts; London, England
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module RL.GPI
  ( Pfloat (..)
  , RLType (..)
  , rltDef
  , optPol
  , maxAndNonZero
  , chooseAndCount
  , poisson'
  , withinOnM
  ) where

import qualified Prelude as P
import Prelude (Show(..), String)
import Protolude  hiding (show, for)

import GHC.TypeNats

import qualified Data.Vector.Sized   as VS

import Control.Monad.Writer
import Data.Finite
import Data.Finite.Internal
import Data.List             ((!!), lookup)
import Data.MemoTrie
import Text.Printf

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

-- | Abstract data type, to future proof API.
data RLType s a n = RLType
  { gamma      :: Float  -- discount rate
  , epsilon    :: Float  -- convergence tolerance
  , maxIter    :: Int    -- max. eval. iterations (0 = Value Iteration)
  , states     :: VS.Vector (n + 1) s
  , actions    :: s -> [a]
  , nextStates :: s -> a -> [s]
  , rewards    :: s -> a -> s -> [(Float,Float)]
  , stateVals  :: [(s, Float)]
  }

rltDef :: RLType s a n
rltDef = RLType
  { gamma     = 1
  , epsilon   = 0.1
  , maxIter   = 10
  , stateVals = []
  }

-- | Yields a single policy improvment iteration, given:
--   - gamma           - discount rate
--   - eps             - convergence tolerance
--   - n               - max. policy evaluation iterations
--   - S               - set of all possible system states
--   - A(s)            - all possible actions in state s,
--   - S'(s, a)        - all possible next states, for a given state/action pair, and
--   - R(s, a, s')     - all possible rewards for a given state/action/next-state triple,
--                       along with their probabilities of occurence.
--
-- Returns a combined policy & value function.
optPol :: ( Eq s, HasTrie s
          , HasTrie a
          , KnownNat (n + 1)
          )
       => RLType s a n               -- ^ abstract type, to protect API
       -> (s -> (a, Float), String)  -- ^ initial policy & value functions
       -> (s -> (a, Float), String)
optPol RLType{..} (g, _) = (bestA, msg)
 where
  bestA   = maximumBy (compare `on` snd) . aVals v'
  aVals v = \s -> let actVal'' = actVal' v
                   in [ (a, actVal'' (s, a))
                      | a <- actions s
                      ]
  actVal' = memo . uncurry . actVal
  actVal v s a =
    fromMaybe
      (sum [ pt * gamma * u + rt
           | s' <- nextStates s a
           , let u = v s'
           , let (pt, rt) = foldl' prSum (0,0)
                                  [ (p, p * r)
                                  | (r, p) <- rs' s a s'
                                  ]
           ])
      $ lookup s stateVals
  prSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)
  rs' = memo3 rewards
  ((_, v'), msg) =
    if length evalIters == 1
      then ((VS.replicate 0, P.head evalIters), "Value Iteration")
      else
        first (fromMaybe (P.error "optPol: Major blow-up!"))
          $ runWriter
            $ withinOnM
                epsilon
                ( chooseAndCount
                    max
                    (> epsilon)
                    "- Found %3d state value diffs > epsilon.  \n"
                . fst
                )
                $ zip (map abs $ zipWith (-) vs (P.tail vs))
                      (P.tail evalIters)
  vs = map (vsFor states) evalIters
  evalIters = take (maxIter + 1) $ iterate (evalPol (fst . g)) $ snd . g
  evalPol p v = let actVal'' = actVal' v
                 in \s -> actVal'' (s, p s)

{----------------------------------------------------------------------
  Misc.
----------------------------------------------------------------------}

-- | Return the maximum value of a set, as well as a scripted log
-- message, regarding the number of non-zero elements in the set.
--
-- (See documentation for `chooseAndCount` function.)
maxAndNonZero :: (Foldable t, Num a, Ord a) => String -> t a -> Writer String a
maxAndNonZero = chooseAndCount max (/= 0)

-- | Choose a value from the set using the given comparison function,
-- and provide a scripted log message, regarding the number of elements
-- in the set meeting the given criteria.
chooseAndCount :: (Foldable t, Num a)
               => (a -> a -> a)  -- ^ choice function
               -> (a -> Bool)    -- ^ counting predicate
               -> String         -- ^ message script (Should contain precisely 1 "%d".)
               -> t a            -- ^ foldable set of elements to count/compare
               -> Writer String a
chooseAndCount f p s xs = do
  let (val, cnt::Int) =
        foldl' ( \ (v, c) x ->
                   ( f v x
                   , if p x
                       then c + 1
                       else c
                   )
               ) (0,0) xs
  tell $ printf s cnt
  return val

--- | To control the formatting of printed floats in output matrices.
newtype Pfloat = Pfloat { unPfloat :: Float}
  deriving (Eq)

instance Show Pfloat where
  show x = printf "%4.1f" (unPfloat x)

poisson :: Int -> Int -> Float
poisson lambda n = lambda' ^ n * exp (-lambda') / fromIntegral (fact n)
 where lambda' = fromIntegral lambda

fact :: Int -> Int
fact 0 = 1
fact n = product [1..n]

poissonVals :: VS.Vector 5 (VS.Vector 12 Float)
poissonVals = VS.generate (VS.generate . poisson)

poisson' :: Finite 5 -> Finite 21 -> Float
poisson' n x@(Finite x') =
  if x > 11  -- The Python code enforces this limit. And we're trying
    then 0   -- for an "apples-to-apples" performance comparison.
    else poissonVals `VS.index` n `VS.index` finite x'

-- | First list element less than or equal to given threshold under the
-- given function, or the last element if the threshold was never met.
--
-- A return value of 'Nothing' indicates an empty list was given.
-- withinOn :: Float -> (a -> Float) -> [a] -> Maybe a
-- withinOn eps f xs = do
--   n <- withinIx eps $ map f xs
--   return $ xs !! n

-- | Index of first list element less than or equal to given threshold,
-- or the index of the last element if the threshold was never met.
--
-- A return value of 'Nothing' indicates an empty list was given.
-- withinIx :: Float -> [Float] -> Maybe Int
-- withinIx _   [] = Nothing
-- withinIx eps xs = Just $ withinIx' 0 xs
--  where withinIx' n []     = n - 1
--        withinIx' n (y:ys) = if y <= eps
--                               then n
--                               else withinIx' (n+1) ys

-- | Monadically search list for first element less than or equal to
-- given threshold under the given function, and return the last element
-- if the threshold was never met.
-- Return 'Nothing' if the input list was empty.
withinOnM :: Monad m
          => Float
          -> (a -> m Float)
          -> [a]
          -> m (Maybe a)
withinOnM _   _ [] = return Nothing
withinOnM eps f xs = do
  n <- withinIxM eps $ map f xs
  case n of
    Nothing -> return Nothing
    Just n' -> return $ Just (xs !! n')

-- | Monadically find index of first list element less than or equal to
-- given threshold, or the index of the last element if the threshold
-- was never met.
--
-- A return value of 'Nothing' indicates an empty list was given.
withinIxM :: Monad m
          => Float
          -> [m Float]
          -> m (Maybe Int)
withinIxM _   [] = return Nothing
withinIxM eps xs = withinIxM' 0 xs
 where withinIxM' n []     = return $ Just (n - 1)
       withinIxM' n (y:ys) = do
         y' <- y
         if y' <= eps then return (Just n)
                      else withinIxM' (n+1) ys

-- | Expected reward for a given state, assuming equiprobable actions.
-- testRewards :: RCState -> Float
-- testRewards s =
--   sum [ uncurry (*) r
--       | a  <- acts
--       , s' <- nextStates s a
--       , r  <- rewards s a s'
--       ] / (fromIntegral . length) acts
--  where acts = actions s

vsFor = flip VS.map

