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
-- Developed while doing the exercises in Ch. 4-6 of the book
-- /Reinforcement Learning: an Introduction/,
--   Richard S. Sutton and Andrew G. Barto
--     The MIT Press
--       Cambridge, Massachusetts; London, England
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -cpp #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RL.GPI where
  -- ( Pfloat (..)
  -- , Pdouble (..)
  -- , RLType (..)
  -- , rltDef
  -- , optPol
  -- , optQ
  -- , doTD
  -- , runEpisode
  -- , maxAndNonZero
  -- , chooseAndCount
  -- , poisson'
  -- , gamma'
  -- , gamma
  -- , withinOnM
  -- , mean
  -- , arrMeanSqr
  -- , qToV
  -- , qToP
  -- , appFv
  -- , appFv
  -- , appFm
  -- , toString
  -- ) where

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
import Data.List                     ((!!), lookup, groupBy, unzip5)
import Data.List.Extras.Argmax       (argmax)
import Data.MemoTrie
import Statistics.Distribution       (density)
import Statistics.Distribution.Gamma (gammaDistr)
import System.Random
import ToolShed.System.Random        (shuffle)
import Text.Printf

import ConCat.TArr

{----------------------------------------------------------------------
  Orphans
----------------------------------------------------------------------}

instance (KnownNat n) => HasTrie (Finite n) where
  data Finite n :->: x     = FiniteTrie (VS.Vector n x)
  trie f                   = FiniteTrie (VS.generate (f . finite . fromIntegral))
  untrie (FiniteTrie v)    = VS.index v
  enumerate (FiniteTrie v) = map (first (finite . fromIntegral)) $ (VS.toList . VS.indexed) v

{----------------------------------------------------------------------
  Markov Decision Process (MDP) Definition.
----------------------------------------------------------------------}

-- | Markov Decision Process (MDP) (finite discrete)
--
-- Laws:
--
-- 1. 1 == sum . map snd $
--           [ jointPMF s a
--           | s <- states
--           , a <- actions s
--           ]
--
-- Usage notes:
--
-- 1. If @initStates@ is empty
--    then use `head states` as the initial state;
--    else randomly select initial state from @initStates@.
--
-- 2. The notation used in the commentary comes from the Sutton & Barto
--    text.
--
class MDP s a where
  type RewardT s a :: *  -- ^ Reward type.
  -- | State enumeration function - S.
  states :: [s]
  -- | Action enumeration function - A(s).
  actions :: s -> [a]
  -- | Joint probability distribution - Pr[(s', r) | s, a].
  jointPMF :: s -> a -> [((s, RewardT s a), Double)]
  -- | Initial states.
  initStates :: [s]
  initStates = []
  -- | Terminal states and their values.
  termStates :: [(s, RewardT s a)]
  termStates = []

{----------------------------------------------------------------------
  Helper functions applicable only to MDPs.
----------------------------------------------------------------------}

-- | Next states and their probabilities - S'(s, a).
nextStates
  :: (MDP s a, Eq s, Ord s)
  => s -> a -> [(s, Double)]
nextStates s a = combProb . map (first fst) $ jointPMF s a

-- | Rewards and their probabilities - R(s, a, s').
rewards
  :: (MDP s a, Eq s, Eq (RewardT s a), Ord (RewardT s a))
  => s -> a -> s -> [((RewardT s a), Double)]
rewards s a s' = combProb . map (first snd)
  . filter ((== s') . fst . fst) $ jointPMF s a

{----------------------------------------------------------------------
  Helper functions expected to be used only locally.
----------------------------------------------------------------------}

-- Eliminate duplicates from a probability distribution, by combining
-- like terms and summing their probabilities.
combProb
  :: (Eq a, Ord a)
  => [(a, Double)] -> [(a, Double)]
combProb =
  map ((fst . P.head) &&& (sum . map snd))
  . groupBy ((==)    `on` fst)
  . sortBy  (compare `on` fst)

{----------------------------------------------------------------------
  General policy iterator
----------------------------------------------------------------------}

-- | Hyper-parameters
--
-- The stated intents of the various fields are correct for the @optPol@
-- function, which was the first solver written and performs dynamic
-- programming (DP).
--
-- Some fields have overloaded meanings/uses, depending upon whether
-- we're `optPol`ing, `optQ`ing, or ...
-- Individual functions below call out such differences in their header
-- commentary as appropriate.
--
-- The type parameter `r` should be equal to the reward type of the MDP
-- being solved.
data HypParams r = HypParams
  { disc       :: r       -- ^ discount rate
  , epsilon    :: r       -- ^ convergence tolerance
  , alpha      :: r       -- ^ error correction gain
  , beta       :: r       -- ^ epsilon/alpha decay factor
  , maxIter    :: Int     -- ^ max. value function evaluation iterations
  , tdStepType :: TDStep  -- ^ temporal difference step type
  }

-- | A default value of type `HypParams`, to use as a starting point.
hypParamsDef :: HypParams Double
hypParamsDef = HypParams
  { disc       =  1
  , epsilon    =  0.1
  , alpha      =  0.1
  , beta       =  0    -- No decay by default.
  , maxIter    = 10
  , tdStepType = Qlearn
  }

-- | Type of temporal difference (TD) step.
data TDStep = Sarsa
            | Qlearn
            | ExpSarsa

-- | Abstract type for capturing debugging information.
data Dbg s r = Dbg
  { nxtStPrbs :: [(s, Double)]
  , curSt     :: s
  , nxtSt     :: s
  , rwd       :: r
  , eNxtVal   :: r
  , termQs    :: [[r]]
  } deriving (Show)

-- | Abstract type of return value from `doTD` function.
data TDRetT s a r = TDRetT
  { valFuncs :: [s -> r]
  , polFuncs :: [s -> a]
  , polXCnts :: [Int]
  , valErrs  :: [r]
  , debugs   :: [[Dbg s r]]
  }
  -- } deriving (Show)

-- | Find optimum policy and value functions, using temporal difference.
--
-- This is intended to be the function called by client code wishing to
-- perform temporal difference based policy optimization.
doTD
  :: ( MDP s a, Num (RewardT s a), HasFin s, HasFin a )  -- , Ord r )  -- , Eq s, Eq a)
  => HypParams (RewardT s a)  -- ^ Simulation hyper-parameters.
  -> Int          -- ^ Number of episodes to run.
  -> TDRetT s a (RewardT s a)
doTD hParams@HypParams{..} nIters = TDRetT vs ps pDiffs vErrs dbgss where
  gen = mkStdGen 1
  (qs, dbgss) = unzip $ flip evalState (VS.replicate (VS.replicate 0), gen, 0) $
    replicateM nIters $ do
      (q, g, t) <- get
      let s = case initStates of
                [] -> P.head states
                _  -> P.head $ shuffle g initStates
          (_, q', g', dbgs, t') = optQn hParams (s, q, g, [], t)
      put (q', g', t')
      return (q', dbgs)
  -- Calculate policies and count differences between adjacent pairs.
  ps     = map (\q -> \s -> argmax (appFm q s) $ actions s) qs
  ass    = map (\p -> map p states) ps
  pDiffs = map (length . filter (== False)) $ zipWith (zipWith (==)) ass (P.tail ass)
  -- Calculate value functions and mean square differences between adjacent pairs.
  vs     = map (\q -> \s -> maximum (appFm q s) $ actions s) qs
  valss  = map (\v -> map v states) vs
  vErrs  = map mean $ zipWith (zipWith (sqr . (-))) valss (P.tail valss)

-- | Yields a single episodic action-value improvement iteration, using n-step TD.
--
-- HypParams field overrides:
-- - epsilon: Used to form an "epsilon-greedy" policy.
-- - maxIter: The "n" in n-step.
optQn
  :: forall s a m n k g.
     ( MDP s a, HasFin s, HasFin a  -- , Eq s, Eq a
     , KnownNat m, KnownNat n, KnownNat k, m ~ (k + 1)
     -- , m ~ Card s, n ~ Card a
     , Card s ~ m, Card a ~ n
     , RandomGen g
     )
  => HypParams (RewardT s a)  -- ^ Simulation hyper-parameters.
  -> (s, Vector m (Vector n (RewardT s a)), g, [Dbg s (RewardT s a)], Integer)
  -> (s, Vector m (Vector n (RewardT s a)), g, [Dbg s (RewardT s a)], Integer)
optQn HypParams{..} (s0, q, gen, _, t) =
  (P.last sNs, q', gen', debgs, t + fromIntegral maxIter)
 where
  (sNs, q', gen', debgs, _) =
    execState (traverse go [(-maxIter + 1)..(maxIter)])
              ([s0], q, gen, [], [])
  go = \ n -> do  -- Single state transition with potential update to Q.
    (ss, qm, g, dbgs, rs) <- get
    let qm' = if n >= 0  -- Only begin updating Q after `maxIter` state transitions.
                then let sNum = toFin $ P.head $ drop n ss
                      in qm VS.// [ ( sNum
                                    , qm `VS.index` sNum VS.// [(toFin a, newVal)]
                                    )
                                  ]
                else qm

        -- Helpful abbreviations
        qf = appFm @s @a qm
        s  = P.last ss

        -- Policy definitions
        (x, g') = random g
        pol' st = argmax (qf st) (actions st)  -- greedy
        pol  st = if x > epsilon'              -- epsilon-greedy
                    then pol' st
                    else case otherActs of
                           [] -> pol' st
                           xs -> P.head $ shuffle gen $ xs
         where otherActs = filter (/= (pol' st)) $ actions st

        -- Use epsilon-greedy policy to choose action.
        a = pol s

        -- Produce a sample next state, obeying the actual distribution.
        s' = P.head $ shuffle gen' $
               concat [ replicate (round $ 100 * p) st
                      | (st, p) <- s'p's
                      ]
        s'p's  = zip s's p's
        s's    = nextStates s a
        p's    = map (sum . map snd) rss  -- next state probabilities
        rss    = map (rewards s a . fst) s's

        -- Do the update.
        newVal =
          fromMaybe
            (oldVal + alpha' * ( sum (take maxIter $ drop n rs)
                               + disc * eQs'a'
                               - oldVal
                               )
            ) $ lookup s termStates
        oldVal = qf s a
        r      = (/ ps') . sum . map (uncurry (*)) $ rewards s a s'
        ps'    = fromMaybe (P.error "RL.GPI.optQ: Lookup failure at line 334!")
                           (lookup s' s'p's)
        eQs'a' =
          case tdStepType of
            Sarsa    -> qf s' $ pol  s'  -- Uses epsilon-greedy policy.
            Qlearn   -> qf s' $ pol' s'  -- Uses greedy policy.
            ExpSarsa ->  -- E[Q(s', a')]
              sum [ p * qf s' a'
                  | a' <- acts
                  , let p = if a' == pol' s'
                               then 1 - epsilon'
                               else epsilon' / fromIntegral (lActs - 1)
                  ]
        acts  = actions s'
        lActs = length acts
        dbg   = Dbg s'p's s s' r eQs'a' $
                  [ [ qf st act
                    | act <- actions st
                    ]
                  | (st, _) <- termStates
                  ]
        alpha'    = alpha   / decayFact
        epsilon'  = epsilon / decayFact
        decayFact = (1 + beta * fromIntegral t)

    put (ss ++ [s'], qm', g', dbgs ++ [dbg], rs ++ [r])

-- | Yields a single policy improvment iteration.
--
-- RLType field overrides:
-- - maxIter: max. policy evaluation iterations (0 = Value Iteration)
--
-- Returns a combined policy & value function.
optPol :: ( MDP s a, Eq s, HasTrie s
          , Ord a, HasTrie a
          , KnownNat m, KnownNat n
          )
       => HypParams r           -- ^ Simulation hyper-parameters.
       -> (s -> (a, r), [Int])  -- ^ initial policy & value functions
       -> (s -> (a, r), [Int])
optPol HypParams{..} (g, _) = (bestA, cnts)
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
            -- | s' <- nextStates s a
            | s' <- map fst $ nextStates s a
            , let u = v s'
            , let (pt, rt) = foldl' prSum (0,0)
                                    [ (p, p * r)
                                    | (r, p) <- rs' s a s'
                                    ]
            ]
      )
      $ lookup s termStates
  prSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)
  rs' = memo3 rewards
  ((_, v'), cnts) =  -- `cnts` is # of value changes > epsilon.
    if length evalIters == 1
      -- then ((VS.replicate 0, P.head evalIters), [])
      then (([], P.head evalIters), [])
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
  vs = map (for states) evalIters
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

#if 0
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
  => RLType s a m n                                        -- ^ abstract type, to protect API
  -> (s, Vector m (Vector n Double), g, Dbg s g, Integer)  -- ^ initial state, action-value matrix, and random generator
  -> (s, Vector m (Vector n Double), g, Dbg s g, Integer)  -- ^ updated state, action-value matrix, and random generator
optQ RLType{..} (s, q, gen, _, t) = (s', q', gen', dbgs, t + 1) where
  q' =
    q VS.// [ ( sNum
              , q `VS.index` sNum VS.// [(aEnum a, newVal)]
              )
            ]

  -- Helpful abbreviations
  sNum = sEnum s
  qf   = appFm q

  -- Policy definitions
  (x, gen') = random gen
  pol' st   = argmax (qf st) (actions st)  -- greedy
  pol  st   = if x > epsilon'              -- epsilon-greedy
                 then pol' st
                 else case otherActs of
                        [] -> pol' st
                        xs -> P.head $ shuffle gen $ xs
   where otherActs = filter (/= (pol' st)) $ actions st

  -- Use epsilon-greedy policy to choose action.
  a = pol s

  -- Produce a sample next state, obeying the actual distribution.
  s' = P.head $ shuffle gen' $
         concat [ replicate (round $ 100 * p) st
                | (st, p) <- s'p's
                ]
  s'p's  = zip s's p's
  s's    = nextStates s a
  p's    = map (sum . map snd) rss  -- next state probabilities
  rss    = map (rewards s a) s's

  -- Do the update.
  newVal =
    fromMaybe
      (oldVal + alpha' * (r + disc * eQs'a' - oldVal)) $
      lookup s termStates
  oldVal = qf s a
  r      = (/ ps') . sum . map (uncurry (*)) $ rewards s a s'
  ps'    = fromMaybe (P.error "RL.GPI.optQ: Lookup failure at line 334!")
                     (lookup s' s'p's)
  eQs'a' =
    case tdStepType of
      Sarsa    -> qf s' $ pol  s'  -- Uses epsilon-greedy policy.
      Qlearn   -> qf s' $ pol' s'  -- Uses greedy policy.
      ExpSarsa ->  -- E[Q(s', a')]
        sum [ p * qf s' a'
            | a' <- acts
            , let p = if a' == pol' s'
                         then 1 - epsilon'
                         else epsilon' / fromIntegral (lActs - 1)
            ]
  acts  = actions s'
  lActs = length acts
  dbgs  = Dbg s'p's s s' r eQs'a' gen $
            [ [ qf st act
              | act <- actions st
              ]
            | (st, _) <- termStates
            ]
  alpha'    = alpha   / decayFact
  epsilon'  = epsilon / decayFact
  decayFact = (1 + beta * fromIntegral t)
#endif

-- | Apply the matrix representation of a function.
--
-- Note that `r` here signifies row, not reward.
appFm
  :: ( HasFin r,   HasFin c
     , KnownNat m, KnownNat n
     , Card r ~ m, Card c ~ n
     )
  => Vector m (Vector n a)  -- ^ matrix representation of f(r,c)
  -> r                      -- ^ row
  -> c                      -- ^ column
  -> a
appFm f r c = f `VS.index` toFin r `VS.index` toFin c

-- | Apply the vector representation of a function.
appFv
  :: ( HasFin x , KnownNat n , Card x ~ n )
  => Vector n a  -- ^ vector representation of f(x)
  -> x           -- ^ function argument
  -> a
appFv f x = f `VS.index` toFin x

-- | Convert an action-value matrix to a value vector.
--
-- (i.e. - Q(s,a) -> V(s))
qToV
  :: ( Ord a
     , KnownNat m, KnownNat n, KnownNat k
     , n ~ (k + 1)
     )
  => Vector m (Vector n a)  -- ^ matrix representation of Q(s,a)
  -> Vector m a             -- ^ matrix representation of V(s)
qToV = VS.map VS.maximum

-- | Convert an action-value matrix to a policy vector.
--
-- (i.e. - Q(s,a) -> A(s))
qToP
  :: ( Ord a, HasFin act
     , KnownNat m, KnownNat n, KnownNat k
     , n ~ (k + 1), Card act ~ n
     )
  => Vector m (Vector n a)  -- ^ matrix representation of Q(s,a)
  -> Vector m act           -- ^ vector representation of P(s)
qToP = VS.map (unFin . VS.maxIndex)

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

-- | Convert any showable type to a string, avoiding the introduction
-- of extra quotation marks when that type is a string to begin with.
toString :: (Show a, Typeable a) => a -> P.String
toString x = fromMaybe (show x) (cast x)

for = flip map

