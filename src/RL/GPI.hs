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
  , Pdouble (..)
  , RLType (..)
  , rltDef
  , optPol
  , maxAndNonZero
  , chooseAndCount
  , poisson'
  , gamma'
  , gamma
  , withinOnM
  , mean
  ) where

import qualified Prelude as P
import Prelude (Show(..))
import Protolude  hiding (show, for)

import qualified Data.Vector.Sized   as VS

import Control.Monad.Writer
import Data.Finite
import Data.Finite.Internal
import Data.List                     ((!!), lookup, groupBy)
import Data.MemoTrie
import Statistics.Distribution       (density)
import Statistics.Distribution.Gamma (gammaDistr)
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
  { disc       :: Double         -- discount rate
  , epsilon    :: Double         -- convergence tolerance
  , maxIter    :: Int            -- max. eval. iterations (0 = Value Iteration)
  , states     :: VS.Vector n s
  , actions    :: s -> [a]
  , nextStates :: s -> a -> [s]
  , rewards    :: s -> a -> s -> [(Double,Double)]
  , stateVals  :: [(s, Double)]  -- overides for terminal states
  }

rltDef :: RLType s a n
rltDef = RLType
  { disc      = 1
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
          , Ord a, HasTrie a
          , KnownNat n
          )
       => RLType s a n               -- ^ abstract type, to protect API
       -> (s -> (a, Double), [Int])  -- ^ initial policy & value functions
       -> (s -> (a, Double), [Int])
optPol RLType{..} (g, _) = (bestA, cnts)
 where
  -- bestA   = maximumBy (compare `on` snd) . aVals v'  --This one just searched for the maximum value.
  bestA   = minimumBy (compare `on` fst)  -- This one groups by value and, within the max. value group,
            . P.last                      -- selects the minimum action. This change was motivated by
            . groupBy ((==) `on` snd)     -- a warning, re: instability, in the text.
            . sortBy (compare `on` snd) . aVals v'
  aVals v = \s -> let actVal'' = actVal' v
                   in [ (a, actVal'' (s, a))
                      | a <- actions s
                      ]
  actVal' = memo . uncurry . actVal
  actVal v s a =
    fromMaybe  -- We look for terminal states first, before performing the default action.
      ( sum [ pt * disc * u + rt
            | s' <- nextStates s a
            , let u = v s'
            , let (pt, rt) = foldl' prSum (0,0)
                                    [ (p, p * r)
                                    | (r, p) <- rs' s a s'
                                    ]
            ]
      )
      $ lookup s stateVals
  prSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)
  rs' = memo3 rewards
  ((_, v'), cnts) =  -- `cnts` is # of value changes > epsilon.
    if length evalIters == 1
      then ((VS.replicate 0, P.head evalIters), [])
      else
        first (fromMaybe (P.error "optPol: Major blow-up!"))
          $ runWriter
            $ withinOnM
                epsilon
                ( chooseAndCount
                    max
                    (> epsilon)
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

--- | To control the formatting of printed floats in output matrices.
newtype Pfloat = Pfloat { unPfloat :: Float}
  deriving (Eq)

instance Show Pfloat where
  show x = printf "%4.1f" (unPfloat x)

newtype Pdouble = Pdouble { unPdouble :: Double }
  deriving (Eq, Ord)

instance Show Pdouble where
  show x = printf "%4.1f" (unPdouble x)

poisson :: Finite 5 -> Finite 12 -> Float
poisson (Finite lambda) (Finite n') =
  lambda' ^ n * exp (-lambda') / fromIntegral (fact n)
 where lambda' = fromIntegral lambda
       n       = fromIntegral n'

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

-- | Gamma pdf
--
-- Assuming `scale = 1`, `shape` should be: 1 + mean.
gammaPdf :: Double -> Double -> Double -> Double
gammaPdf shape scale = density (gammaDistr shape scale)

-- | Gamma pmf
--
-- Scale assumed to be `1`, so as to match the calling signature of
-- `poisson`.
gamma :: Finite 5 -> Finite 12 -> Double
gamma (Finite expect') (Finite n') =
  0.1 * sum [gammaPdf' (n + x) | x <- [0.1 * m | m <- [-4..5]]]
    where gammaPdf' = gammaPdf (1 + expect) 1
          expect    = fromIntegral expect'
          n         = fromIntegral n'

gammaVals :: VS.Vector 5 (VS.Vector 12 Double)
gammaVals = VS.generate (VS.generate . gamma)

gamma' :: Finite 5 -> Finite 21 -> Double
gamma' n (Finite x') =
  if x' > 11
    then 0
    else gammaVals `VS.index` n `VS.index` finite x'

-- | Monadically search list for first element less than
-- given threshold under the given function, and return the last element
-- if the threshold was never met.
-- Return 'Nothing' if the input list was empty.
withinOnM :: Monad m
          => Double
          -> (a -> m Double)
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
          => Double
          -> [m Double]
          -> m (Maybe Int)
withinIxM _   [] = return Nothing
withinIxM eps xs = withinIxM' 0 xs
 where withinIxM' n []     = return $ Just (n - 1)
       withinIxM' n (y:ys) = do
         y' <- y
         if y' < eps then return (Just n)
                     else withinIxM' (n+1) ys

-- | Return the maximum value of a set, as well as a count of the number
-- of non-zero elements in the set.
--
-- (See documentation for `chooseAndCount` function.)
maxAndNonZero :: (Foldable t, Num a, Ord a) => t a -> Writer [Int] a
maxAndNonZero = chooseAndCount max (/= 0)

-- | Choose a value from the set using the given comparison function,
-- and provide a count of the number of elements in the set meeting the
-- given criteria.
chooseAndCount :: (Foldable t, Num a)
               => (a -> a -> a)  -- ^ choice function
               -> (a -> Bool)    -- ^ counting predicate
               -> t a            -- ^ foldable set of elements to count/compare
               -> Writer [Int] a
chooseAndCount f p xs = do
  let (val, cnt::Int) =
        foldl' ( \ (v, c) x ->
                   ( f v x
                   , if p x
                       then c + 1
                       else c
                   )
               ) (0,0) xs
  tell [cnt]
  return val

vsFor = flip VS.map

-- | Mean value of a collection
mean :: (Foldable f, Fractional a) => f a -> a
mean = uncurry (/) . second fromIntegral . foldl' (\ (!s, !n) x -> (s+x, n+1)) (0,0::Integer)

