-----------------------------------------------------------------------------
-- |
-- Module      :  RL.Util
-- Description :  Misc. Utilities
-- Copyright   :  (c) Target Corp., 2018
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David.Banas@target.com
-- Stability   :  experimental
-- Portability :  ?
--
-- Common utilities used throughout the RL package.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module RL.Util where

import qualified Prelude as P
import Prelude (Show(..))
import Protolude  hiding (show, for)

import GHC.TypeLits

import qualified Data.Vector.Sized   as VS
import Data.Vector.Sized             (Vector)

import Control.Arrow                 ((&&&))
import Control.Monad.Writer
import Data.Finite
import Data.Finite.Internal
import Data.List                     ((!!), groupBy)
import Statistics.Distribution       (density)
import Statistics.Distribution.Gamma (gammaDistr)
import Text.Printf

import ConCat.TArr

-- | Convenient abbreviation of: `natVal (Proxy @n)`.
nat :: forall n. KnownNat n => Integer
nat = natVal (Proxy @n)

-- | Convenient abbreviation of: `fromIntegral (nat @n)`.
int :: forall n. KnownNat n => Int
int = fromIntegral (nat @n)

type Unop a = a -> a

-- | Apply the matrix representation of a two argument function.
--
-- The first argument is assumed to index the rows of the matrix.
appFm
  :: ( HasFin' a,   HasFin' b )
  => Vector (Card a) (Vector (Card b) c)  -- ^ matrix representation of @f(r,c)@
  -> a                                    -- ^ row
  -> b                                    -- ^ column
  -> c
appFm f x y = f `VS.index` toFin x `VS.index` toFin y

-- | Apply the vector representation of a function.
appFv
  :: ( HasFin' x )
  => Vector (Card x) a  -- ^ vector representation of @f(x)@
  -> x                  -- ^ function argument
  -> a
appFv f x = f `VS.index` toFin x

-- | Convert an action-value matrix to a value vector.
--
-- (i.e. - \(Q(s,a) -> V(s)\))
qToV
  :: ( Ord a
     , KnownNat m, KnownNat n, KnownNat k
     , n ~ (k + 1)
     )
  => Vector m (Vector n a)  -- ^ matrix representation of @Q(s,a)@
  -> Vector m a             -- ^ vector representation of @V(s)@
qToV = VS.map VS.maximum

-- | Convert an action-value matrix to a policy vector.
--
-- (i.e. - \(Q(s,a) -> A(s)\))
qToP
  :: ( Ord a, HasFin' act
     , KnownNat m, KnownNat n, KnownNat k
     , n ~ (k + 1), Card act ~ n
     )
  => Vector m (Vector n a)  -- ^ matrix representation of Q(s,a)
  -> Vector m act           -- ^ vector representation of P(s)
qToP = VS.map (unFin . VS.maxIndex)

{----------------------------------------------------------------------
  Misc.
----------------------------------------------------------------------}

-- | To control the formatting of printed floats in output matrices.
newtype Pfloat = Pfloat { unPfloat :: Float}
  deriving (Eq)

instance Show Pfloat where
  show x = printf "%5.2f" (unPfloat x)

-- | To control the formatting of printed doubles in output matrices.
newtype Pdouble = Pdouble { unPdouble :: Double }
  deriving (Eq, Ord)

instance Show Pdouble where
  show x = printf "%5.2f" (unPdouble x)

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

for :: [a] -> (a -> b) -> [b]
for = flip map

vsFor :: Vector n a -> (a -> b) -> Vector n b
vsFor = flip VS.map

-- | Mean value of a collection
mean :: (Foldable f, Fractional a) => f a -> a
mean = uncurry (/) . second fromIntegral . foldl' (\ (!s, !n) x -> (s+x, n+1)) (0,0::Integer)

-- | Find the mean square of a list of lists.
arrMeanSqr :: (Functor f, Foldable f, Functor g, Foldable g, Fractional a) => f (g a) -> a
arrMeanSqr = mean . fmap mean . fmap (fmap sqr)

-- | Take the square of a numerical type.
sqr :: Num a => a -> a
sqr x = x * x

-- | Convert any showable type to a string, avoiding the introduction
-- of extra quotation marks when that type is a string to begin with.
toString :: (Show a, Typeable a) => a -> P.String
toString x = fromMaybe (show x) (cast x)

-- | Convert a Bool to a Double.
boolToDouble :: Bool -> Double
boolToDouble True = 1
boolToDouble _    = 0

-- | Convert a Bool to a Int.
boolToInteger :: Bool -> Integer
boolToInteger True = 1
boolToInteger _    = 0

-- | Convert a Int to a Bool.
integerToBool :: Integer -> Bool
integerToBool 1 = True
integerToBool _ = False

-- | Take every nth element from a list.
takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n xs = P.head xs : (takeEvery n $ drop n xs)

-- | Calculate a moving window average over a collection.
winMean :: Fractional a => Int -> [a] -> [a]
winMean _ [] = []
winMean n xs = map (/ (fromIntegral n')) $ initSum : winMean' initSum xs (drop n xs)
 where
  n'      = min (length xs) n
  initSum = sum $ take n' xs

winMean' :: Fractional a => a -> [a] -> [a] -> [a]
winMean' _   _    []   = []
winMean' acc subs adds = acc' : (winMean' acc' (P.tail subs) (P.tail adds))
 where
  acc' = acc - (P.head subs) + (P.head adds)

-- | Eliminate duplicates from a probability distribution, by combining
-- like terms and summing their probabilities.
combProb
  :: (Eq a, Ord a)
  => Unop [(a, Double)]  
combProb =
  map ((fst . P.head) &&& (sum . map snd))
  . groupBy ((==)    `on` fst)
  . sortBy  (compare `on` fst)
