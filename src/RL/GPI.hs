-----------------------------------------------------------------------------
-- |
-- Module      :  RL.GPI
-- Description :  General Policy Iterator
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
--   Richard S. Sutton and Andrew G. Barto,
--     The MIT Press,
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

import qualified Prelude as P
import Prelude (Show(..))
import Protolude  hiding (show, for)

import qualified Data.Vector.Sized   as VS
import Data.Vector.Sized             (Vector)

-- import Control.Monad.Loops
import Control.Monad.Writer
import Data.Finite
import Data.List                     (lookup, groupBy, unzip5, zip3)
import Data.List.Extras.Argmax       (argmax)
import Data.MemoTrie
import System.Random
import Text.Printf
import ToolShed.System.Random        (shuffle)

import ConCat.TArr

import RL.MDP
import RL.Util

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

-- | Hyper-parameters
--
-- The stated intents of the various fields are correct for the @'optPol'@
-- function, which was the first solver written and performs dynamic
-- programming (DP).
--
-- Some fields have overloaded meanings/uses, depending upon whether
-- we're @optPol@ing, @optQ@ing, or ...
-- Individual functions below call out such differences in their header
-- commentary as appropriate.
--
-- The type parameter @r@ should be equal to the reward type of the MDP
-- being solved.
data HypParams r = HypParams
  { disc       :: r       -- ^ discount rate
  , epsilon    :: r       -- ^ convergence tolerance
  , alpha      :: r       -- ^ error correction gain
  , beta       :: r       -- ^ epsilon/alpha decay factor
  , maxIter    :: Int     -- ^ max. value function evaluation iterations
  , tdStepType :: TDStep  -- ^ temporal difference step type
  , nSteps     :: Int     -- ^ # of steps in n-step TD
  , mode       :: Mode    -- ^ optimization mode
  }

-- | A default value of type @HypParams@, to use as a starting point.
hypParamsDef :: HypParams Double
hypParamsDef = HypParams
  { disc       =  1
  , epsilon    =  0.1
  , alpha      =  0.1
  , beta       =  0    -- No decay by default.
  , maxIter    = 10
  , tdStepType = Qlearn
  , nSteps     = 0     -- TD(0) by default.
  , mode       = TD
  }

-- | Type of temporal difference (TD) step.
data TDStep = Sarsa
            | Qlearn
            | ExpSarsa

-- | Optimization mode.
data Mode = MC
          | TD
  deriving (Show, Eq)

-- | Abstract type for capturing debugging information.
data Dbg s a r = Dbg
  { curSt     :: s
  , act       :: a
  , nxtSt     :: s
  , rwd       :: r
  , nxtStPrbs :: [(s, Double)]
  } deriving (Show)

-- | Abstract type of return value from @'doDP'@ function.
data DPRetT s a r = DPRetT
  { valFunc :: s -> r
  , polFunc :: s -> a
  , valXCnts :: [[Int]]
  , polXCnts' :: [Int]
  }

-- | Abstract type of return value from @'doTD'@ function.
data TDRetT s a r m n = TDRetT
  { valFuncs :: [s -> r]
  , polFuncs :: [s -> a]
  , polXCnts :: [Int]
  , valErrs  :: [r]
  , debugs   :: [[Dbg s a r]]
  , qMats    :: [Vector m (Vector n (Double, Int))]
  }

-- | Find optimum policy and value functions, using /dynamic programming/.
--
-- This is intended to be the function called by client code wishing to
-- perform /dynamic programming/ based policy optimization.
doDP
  :: forall s.
     ( MDP s, Ord s, HasTrie s, HasFin' s
     , Eq (ActionT s), Ord (ActionT s)
     , HasTrie (ActionT s), HasFin' (ActionT s)
     )
  => HypParams (Double)  -- ^ Simulation hyper-parameters.
  -> Int                 -- ^ Number of episodes to run.
  -> DPRetT s (ActionT s) Double
doDP hParams@HypParams{..} nIters = DPRetT val pol valXCnts polXCnts where
  (fs, valXCntss) =
    P.unzip $ take nIters $ iterate (optPol hParams) (const (defAct, 0), [])
  defAct   = P.head $ actions defState
  defState = if length (initStates @s) > 0
               then P.head $ initStates @s
               else P.head states
  valXCnts = P.tail valXCntss
  acts     = map (\f -> map (fst . f) states) fs
  diffs    = map (map boolToDouble . uncurry (zipWith (/=)))
               $ zip acts (P.tail acts)
  g' :: s -> (ActionT s, Double)
  ((_, g'), polXCnts) = first (fromMaybe (P.error "doDP: Major failure!")) $
    runWriter $ withinOnM 0  -- Temporary, to force `nIters` policy improvements.
                  ( \ (dv, _) ->
                      maxAndNonZero dv
                  ) $ zip diffs (P.tail fs)
  pol = fst . g'
  val = snd . g'

-- | Find optimum policy and value functions, using temporal difference.
--
-- This is intended to be the function called by client code wishing to
-- perform /temporal difference/ based policy optimization.
doTD
  :: forall s.
     ( MDP s, Ord s, Eq (ActionT s)
     , HasFin' s, HasFin' (ActionT s)
     )
  => HypParams (Double)  -- ^ Simulation hyper-parameters.
  -> Int                 -- ^ Number of episodes to run.
  -> TDRetT s (ActionT s) (Double) (Card s) (Card (ActionT s))
doTD hParams@HypParams{..} nIters = TDRetT vs ps pDiffs vErrs dbgss qs where
  gen = mkStdGen 1
  (qs, dbgss) = unzip $ flip evalState (VS.replicate (VS.replicate (0, 0)), gen, 0) $
    replicateM nIters $ do
      (q, g, t) <- get
      let s = case initStates @s of
                [] -> P.head states
                _  -> P.head $ shuffle g initStates
          (_, q's, g's, dbgs, t's) =
            unzip5 $ take (maxIter + 1) $
                          iterate (optQn hParams) (s, q, g, [], t)
          q' = P.last q's
      put (q', P.last g's, P.last t's)
      -- return (q', P.last dbgs)
      return (q', concat dbgs)
  -- Calculate policies and count differences between adjacent pairs.
  ps     = map (\q -> \s -> argmax (appFm q s) $ actions s) qs
  ass    = map (\p -> map p states) ps
  pDiffs = map (length . filter (== False)) $ zipWith (zipWith (==)) ass (P.tail ass)
  -- Calculate value functions and mean square differences between adjacent pairs.
  vs     = map (\q -> \s -> maximum . map (fst . appFm q s) $ actions s) qs
  valss  = map (\v -> map v states) vs
  vErrs  = map (mean . map sqr) $ zipWith (zipWith (-)) valss (P.tail valss)

{- | Object of @'optQn'@ function.

Tuple element descriptions:

- Current state.
- Augmented matrix representation of Q(s,a). Cell type: (value, # of adjustments).
- Random number generator.
- List of debugging data structures.
- Simulation time in units of state transitions.
-}
type OptQT s g =
  ( s
  , Vector (Card s) (Vector (Card (ActionT s)) (Double, Int))
  , g
  , [Dbg s (ActionT s) (Double)]
  , Integer
  )

type Unop a = a -> a

{- | Yields a single episodic action-value improvement iteration, using n-step TD.

@'HypParams'@ field overrides:

- @epsilon@: Used to form an "epsilon-greedy" policy.

This function accomodates the /Monte Carlo/ (MC) method by using the
following method of mean calculation, which has the same form as
temporal difference (TD) error correction:

\\[
y = f(x) \\\\
z = g(y)
\\]

-}
optQn
  :: ( MDP s, HasFin' s, Ord s
     , Eq (ActionT s), HasFin' (ActionT s)
     , RandomGen g
     )
  => HypParams (Double)  -- ^ Simulation hyper-parameters.
  -> Unop (OptQT s g)
optQn HypParams{..} (s0, q, gen, _, t) =
  (sN, q VS.// qUpdates, gen', reverse debugs, t + (fromIntegral . length) sts)
 where
  sN = if mode == MC
         then sT
         else if nSteps > 0
                then (P.last . P.init) sts
                else sT
  qUpdates =
    [ ( sNum
      , q `VS.index` sNum VS.// [(aNum, (newVal, visits s a + 1))]
      )
    | (s, a, ret) <- zip3 sts' acts' rets
    , let sNum = toFin s
          aNum = toFin a
          newVal =
            if s `elem` termStates
              then 0
              else oldVal + errGain * (ret - oldVal)
          oldVal = qf s a
          errGain =
            case mode of
              MC -> 1 / (1 + (fromIntegral $ visits s a))  -- See discussion on mean calculation above.
              _  -> alpha'
    ]
  -- Returns are much easier to calculate with reversed rewards.
  rets = P.tail $ scanl (+) (v sT * disc ^ (length rs')) rs'
  rs'  = reverse $ zipWith (*) gammas $ reverse rwds  -- reversed discounted rewards
  sT   = P.head sts
  sts'  = P.tail sts
  acts' = P.tail acts  
  -- State value function corresponding to Q(s,a) depends on mode.
  v st = case mode of
    MC -> 0  -- We assume we reached a terminal state.
    _  ->
      case tdStepType of
        Sarsa    -> qf st $ epsGreedy gen epsilon' qf st  -- Uses epsilon-greedy policy.
        Qlearn   -> qf st $ greedy qf st                  -- Uses greedy policy.
        ExpSarsa ->  -- E[Q(s', a')]
          sum [ p * qf st a
              | a <- as
              , let p = if a == greedy qf st
                           then 1 - epsilon'
                           else epsilon' / fromIntegral (lActs - 1)
              ] where as    = actions st
                      lActs = length acts
  gammas    = scanl (*) 1 $ repeat disc      -- [1, disc, disc^2, ...]
  qf        = ((<$>) . (<$>)) fst $ appFm q  -- From augmented matrix representation of Q(s,a) to actual function.
  visits    = ((<$>) . (<$>)) snd $ appFm q  -- # of visits/adjustments to particular (s,a) pair.
  alpha'    = alpha   / decayFact
  epsilon'  = epsilon / decayFact
  decayFact = (1 + beta * fromIntegral t)
  (sts, acts, rwds, debugs, gen') =
    execState (replicateM (nSteps + 1) go)
              ([s0], [epsGreedy gen epsilon' qf s0], [], [], gen)
  go = do  -- Single state transition.
    (ss, as, rs, dbgs, g) <- get
    let s  = P.head ss
        a  = P.head as
    if s `elem` termStates
      then return ()
      else do
        let -- Produce a sample next state, obeying the actual distribution.
            s' = P.head $ shuffle g $
                   concat [ replicate (round $ 100 * p) st
                          | (st, p) <- s'p's
                          ]
            s'p's = nextStates s a
            -- Calculate reward.
            r   = (/ ps') . sum . map (uncurry (*)) $ rewards s a s'
            ps' = fromMaybe (P.error "RL.GPI.optQn.go: Lookup failure at line 199!")
                            (lookup s' s'p's)
            -- Use a greedy policy to select the next action.
            a' = greedy qf s'
            -- Advance the random number generator.
            (_::Int, g') = random g
            -- TEMPORARY DEBUGGING INFO
            dbg = Dbg s a s' r s'p's
        put (s' : ss, a' : as, r : rs, dbg : dbgs, g')  -- Note the reverse order build-up.
        return ()
      -- if s' `elem` termStates
      -- if s `elem` termStates
      --   then Nothing              -- Short circuit remaining computation,
      --   else Just (s, a, r, dbg)  -- if we've reached a terminal state.

-- \[
-- \begin{eqnarray}
-- \bar{x}^N     &=& \frac{x_0 + x_1 + ... + x_{N-1}}{N} \\\\
-- \bar{x}^{N+1} &=& \frac{x_0 + x_1 + ... + x_N}{N+1} \\\\
-- \bar{x}^{N+1} &=& \frac{N \cdot bar{x}^N + x_N}{N+1} \\\\
-- \bar{x}^{N+1} &=& \frac{(N+1) \cdot bar{x}^N - bar{x}^N + x_N}{N+1} \\\\
-- \bar{x}^{N+1} &=& bar{x}^N + \frac{x_N - bar{x}^N}{N+1} \\\\
-- \bar{x}^{N+1} &=& bar{x}^N + \frac{1}{N+1} \cdot \left( x_N - bar{x}^N \right) \\\\
-- \end{eqnarray}
-- \]
--

-- | Yields a single policy improvment iteration.
--
-- @'HypParams'@ field overrides:
--
-- - @maxIter@: max. policy evaluation iterations (0 = Value Iteration)
--
-- Returns a combined policy & value function.
optPol :: ( MDP s, Eq s, Ord s, HasTrie s
          , Ord (ActionT s), HasTrie (ActionT s)
          )
       => HypParams Double                     -- ^ Simulation hyper-parameters.
       -> (s -> ((ActionT s), Double), [Int])  -- ^ initial policy & value functions
       -> (s -> ((ActionT s), Double), [Int])
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
    if s `elem` termStates
      then 0
      else
        sum [ pt * disc * u + rt
            | s' <- map fst $ nextStates s a
            , let u = v s'
            , let (pt, rt) = foldl' prSum (0,0)
                                    [ (p, p * r)
                                    | (r, p) <- rs' s a s'
                                    ]
            ]
  prSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)
  rs' = memo3 rewards
  ((_, v'), cnts) =  -- `cnts` is # of value changes > epsilon.
    if length evalIters == 1
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
                $ zip (map (map abs) $ zipWith (zipWith (-)) vs (P.tail vs))
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
