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
  , optQ
  , doTD
  , runEpisode
  , maxAndNonZero
  , chooseAndCount
  , poisson'
  , gamma'
  , gamma
  , withinOnM
  , mean
  , arrMeanSqr
  , qToV
  , qToP
  , appV
  , appP
  , appQ
  , toString
  ) where

import qualified Prelude as P
import Prelude (Show(..))
import Protolude  hiding (show, for)

import GHC.TypeLits

import qualified Data.Vector.Sized   as VS
import Data.Vector.Sized             (Vector)

import Control.Monad.Writer
import Data.Finite
import Data.Finite.Internal
import Data.List                     ((!!), lookup, groupBy, unzip3)
import Data.List.Extras.Argmax       (argmax)
import Data.MemoTrie
import Statistics.Distribution       (density)
import Statistics.Distribution.Gamma (gammaDistr)
import System.Random
import ToolShed.System.Random        (shuffle)
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

-- | Abstract data type, to future-proof API.
--
-- Note: Some fields have overloaded meanings/uses, depending upon
--       whether we're `optPol`ing, `optQ`ing, or ...
--       Individual functions, below, call out such differences, in
--       their header commentary, as appropriate.
--
-- Note: The functions in this module presume:
--       Pr[S' = s' | S = s, A = a] = (sum . map snd) R(s, a, s')
data RLType s a m n = RLType
  { disc       :: Double  -- ^ discount rate
  , epsilon    :: Double  -- ^ convergence tolerance
  , alpha      :: Double  -- ^ error correction gain
  , maxIter    :: Int     -- ^ max. iterations of ?

  -- | S - all possible states
  , states     :: VS.Vector m s

  -- | A(s) - all possible actions from state s
  , actions    :: s -> [a]

  -- | S'(s, a) - all possible next states, given current state-action pair
  , nextStates :: s -> a -> [s]

  -- | R(s, a, s') - all possible rewards, along with their probabilities
  -- of occurence, given current state-action pair and next state
  , rewards    :: s -> a -> s -> [(Double,Double)]

  , sEnum      :: s -> Finite m  -- ^ state enumeration function
  , aEnum      :: a -> Finite n  -- ^ action enumeration function
  , sGen       :: Finite m -> s  -- ^ state generation function
  , aGen       :: Finite n -> a  -- ^ action generation function

  , stateVals  :: [(s, Double)]  -- ^ overides for terminal states

  -- | Possible initial states.
  --
  -- If list is empty
  --   then use `head states` as the initial state;
  --   else randomly select initial state from `initStates`.
  , initStates :: [s]
  }

-- | A default value of type `RLType`, to use as a starting point.
--
-- Note that this value is virtual, leaving fields: `states`, `actions`,
-- `nextStates`, `rewards`, `sEnum`, and `aEnum` undefined, and must be
-- completed for a particular MDP before use.
rltDef :: RLType s a m n
rltDef = RLType
  { disc       = 1
  , epsilon    = 0.1
  , alpha      = 0.1
  , maxIter    = 10
  , stateVals  = []
  , initStates = []
  }

-- | Yields a single policy improvment iteration.
--
-- RLType field overrides:
-- - maxIter: max. policy evaluation iterations (0 = Value Iteration)
--
-- Returns a combined policy & value function.
optPol :: ( Eq s, HasTrie s
          , Ord a, HasTrie a
          , KnownNat m, KnownNat n
          )
       => RLType s a m n             -- ^ abstract type, to protect API
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

-- | Run one episode of a MDP, returning the list of states visited.
runEpisode
  :: Eq s
  => Int            -- ^ Max. state transitions
  -> (s -> a)       -- ^ Policy
  -> (s -> a -> s)  -- ^ Next state calculator
  -> [s]            -- ^ Terminal states
  -> s              -- ^ Initial state
  -> [s]
runEpisode n pol nxt terms init =
  takeWhile (`notElem` terms) $
    take n $ iterate (\s -> nxt s (pol s)) init

-- | Yields a single episodic action-value improvement iteration.
--
-- RLType field overrides:
-- - epsilon: Used to form an "epsilon-greedy" policy.
-- - maxIter: (ignored)
-- - states:  First in list is assumed to be the initial state,
--            if `initStates` is empty.
optQ
  :: ( KnownNat m, KnownNat n, KnownNat k
     , m ~ (k + 1)
     , Eq s, Eq a, RandomGen g
     )
  => RLType s a m n                      -- ^ abstract type, to protect API
  -> (s, Vector m (Vector n Double), g)  -- ^ initial state, action-value matrix, and random generator
  -> (s, Vector m (Vector n Double), g)  -- ^ updated state, action-value matrix, and random generator
optQ RLType{..} (s, q, gen) = (s', q', gen') where
  q' =
    q VS.// [ ( sNum
              , q `VS.index` sNum VS.// [(aEnum a, newVal)]
              )
            ]
  sNum = sEnum s

  -- purely greedy policy using initial Q(s,a)
  pol st = argmax (qf st) (actions st)
  qf     = appQ sEnum aEnum q

  -- Use epsilon-greedy policy to choose action.
  (x, gen') = random gen
  a         = if x > epsilon
                 then pol s
                 else P.head $ shuffle gen' $ actions s

  -- Produce a sample next state, obeying the actual distribution.
  s' = P.head $ shuffle gen' $
         concat [ replicate (round $ 100 * p) st
                | (st, p) <- s'p's
                ]
  s'p's  = zip s's p's
  s's    = nextStates s a
  p's    = map (sum . map snd) rss  -- next state probabilities
  rss    = map (rewards s a) s's

  -- temporary hard-wiring to Expected SARSA
  newVal = oldVal + alpha * (r + disc * eQs'a' - oldVal)
  oldVal = qf s a
  r      = sum . map (sum . map (uncurry (*))) $ rss  -- expected reward
  eQs'a' =  -- E[Q(s', a')]
    sum [ p * qf s' a'
        | a' <- acts
        , let p = if a' == pol s'
                     then 1 - epsilon
                     else epsilon / fromIntegral (lActs - 1)
        ]
  acts  = actions s'
  lActs = length acts

-- | Find optimum policy and value functions, using temporal difference.
doTD
  :: ( Eq s, Eq a
     , KnownNat ns, KnownNat na, KnownNat k, KnownNat l
     , ns ~ (1 + k) , (k + 1) ~ ns , na ~ (l + 1)
     ) => RLType s a ns na
       -> Int
       -> ([VS.Vector ns Double], [VS.Vector ns a], [Int], [Int])
doTD rlt@RLType{..} nIters = (vs, ps, polChngCnts, valChngCnts) where
  gen = mkStdGen 1
  qs  = flip evalState (VS.replicate (VS.replicate 0), gen) $
    replicateM nIters $ do
      (q, g) <- get
      let s = case initStates of
                [] -> VS.head states
                _  -> P.head $ shuffle g initStates
          (_, q's, gs) = unzip3 $ take (maxIter + 1) $
            iterate
              (optQ rlt)
              (s, q, g)
          q' = P.last q's
      put (q', P.last gs)
      return q'
  ps    = map (qToP aGen) qs
  acts  = map (\p -> VS.map (getFinite . aEnum . appP sEnum p) states) ps
  diffs = map (VS.map (fromIntegral . abs) . uncurry (-))
              $ zip acts (P.tail acts)
  ((_, _), polChngCnts) = first (fromMaybe (P.error "doTD: Major failure!")) $
    runWriter $ withinOnM 0
                          ( \ (da, _) ->
                              maxAndNonZero da
                          ) $ zip diffs (P.tail ps)
  vs          = map qToV qs
  valChngCnts = map ( length
                    . filter (/= 0)
                    . VS.toList
                    ) $ zipWith (-) vs $ P.tail vs

-- | Apply the matrix representation of an action-value function.
appQ
  :: (KnownNat m, KnownNat n)
  => (s -> Finite m)             -- ^ state enumerator
  -> (a -> Finite n)             -- ^ action enumerator
  -> Vector m (Vector n Double)  -- ^ matrix representation of Q(s,a)
  -> s                           -- ^ state
  -> a                           -- ^ action
  -> Double
appQ sEnum aEnum q s a =
  q `VS.index` sEnum s `VS.index` aEnum a

-- | Apply the matrix representation of a policy.
appP
  :: KnownNat m
  => (s -> Finite m)             -- ^ state enumerator
  -> Vector m a                  -- ^ vector representation of A(s)
  -> s                           -- ^ state
  -> a
appP sEnum p s = p `VS.index` sEnum s

-- | Apply the matrix representation of a value function.
appV
  :: KnownNat m
  => (s -> Finite m)             -- ^ state enumerator
  -> Vector m Double             -- ^ vector representation of V(s)
  -> s                           -- ^ state
  -> Double
appV sEnum v s = v `VS.index` sEnum s

-- | Convert an action-value matrix to a value vector.
--
-- (i.e. - Q(s,a) -> V(s))
qToV
  :: (KnownNat m, KnownNat n, KnownNat k, n ~ (k + 1))
  => Vector m (Vector n Double)  -- ^ matrix representation of Q(s,a)
  -> Vector m Double             -- ^ matrix representation of V(s)
qToV = VS.map VS.maximum

-- | Convert an action-value matrix to a policy vector.
--
-- (i.e. - Q(s,a) -> A(s))
qToP
  :: (KnownNat m, KnownNat n, KnownNat k, n ~ (k + 1))
  => (Finite n -> a)             -- ^ action generator
  -> Vector m (Vector n Double)  -- ^ matrix representation of Q(s,a)
  -> Vector m a                  -- ^ matrix representation of V(s)
qToP aGen = VS.map (aGen . VS.maxIndex)

{----------------------------------------------------------------------
  Misc.
----------------------------------------------------------------------}

-- Unary function type alias, for notational convenience.
-- type Unary a = a -> a

-- | To control the formatting of printed floats in output matrices.
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

-- | Find the mean square of a list of lists.
arrMeanSqr :: (Functor f, Foldable f, Functor g, Foldable g, Fractional a) => f (g a) -> a
arrMeanSqr = mean . fmap mean . fmap (fmap sqr)

sqr :: Num a => a -> a
sqr x = x * x

toString :: (Show a, Typeable a) => a -> P.String
toString x = fromMaybe (show x) (cast x)

