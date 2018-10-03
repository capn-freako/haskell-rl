------------------------------------------------------------------------
-- |
-- Module      :  RL.Markov
-- Description :  Markov Processes
-- Copyright   :  (c) Target Corp., 2018
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David.Banas@target.com
-- Stability   :  experimental
-- Portability :  ?
--
-- Markov Processes
------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE UndecidableInstances #-}

module RL.Markov where

-- import qualified Prelude as P
import Protolude  hiding (show, for)

-- import Control.Arrow                 ((&&&))
-- import Data.List                     (groupBy)
import Data.List.Extras.Argmax       (argmax)
import System.Random
import ToolShed.System.Random        (shuffle)

import RL.Util

-- | Base class for all Markov Processes (finite discrete)
--
-- Usage notes:
--
-- 1. If @initStates@ is empty
--    then use @head states@ as the initial state;
--    else randomly select initial state from @initStates@.
--
-- 2. The notation used in the commentary comes from the Sutton & Barto
--    text.
--
class IsState s where
  -- | State enumeration function - \(S\).
  states :: [s]
  -- | Initial states.
  initStates :: [s]
  initStates = states
  -- | Terminal states.
  termStates :: [s]
  termStates = []

-- | Markov Decision Process (MDP) (finite discrete)
--
-- Laws:
--
-- 1. @jointPMF@ must return a true probability distribution. That is:
--
--     @ 1 = sum . map snd $ jointPMF s a @
--
--     \(, \forall s \in \text{states}, \forall a \in \text{actions s}\)
--
class IsState s => MDP s where
  type ActionT s :: *
  -- | Action enumeration function - \(A(s)\).
  actions :: s -> [ActionT s]
  -- | Joint probability distribution - \(Pr[(s', r) | s, a]\).
  jointPMF :: s -> ActionT s -> [((s, Double), Double)]

-- | Markov Reward Process (MRP) (finite discrete)
--
-- Laws:
--
-- 1. @jointPMF@ must return a true probability distribution. That is:
--
--     @ 1 = sum . map snd $ jointPMF s a @
--
--     \(, \forall s \in \text{states}, \forall a \in \text{actions s}\)
--
class IsState s => MRP s where
  rewardPMF :: s -> [(Double, Double)]

-- | Default MRP instance for MDP types.
instance (IsState s, MDP s) => MRP s where
  rewardPMF s =
    combProb $ map (first snd) $ concat
      [ jointPMF s a
      | a <- actions s
      ]
      
-- | Markov Process (MP) (finite discrete)
--
-- Laws:
--
-- 1. @statePMF@ must return a true probability distribution. That is:
--
--     @ 1 = sum . map snd $ statePMF s @
--
--     \(, \forall s \in \text{states}\)
--
class IsState s => MP s where
  -- | State transition probability distribution - \(Pr[s' | s]\).
  statePMF :: s -> [(s, Double)]

-- | Default MP instance for MDP types.
instance (IsState s, MDP s, Ord s) => MP s where
  statePMF s =
    combProb $ map (first fst) $ concat
      [ jointPMF s a
      | a <- actions s
      ]
      
-- * Helper Functions
--
-- ^ The following functions operate on types that are members of
-- one or more of the typeclasses defined in this module.

-- | Calculate the maximally likely next state.
expectedState
  :: (MP s, Ord s)
  => s -> s
expectedState = fst . maximumBy (compare `on` snd) . combProb . statePMF

-- | Expected reward.
expectedReward
  :: (MRP s)
  => s -> Double
expectedReward = sum . map (uncurry (*)) . rewardPMF

-- | Next states and their probabilities - S'(s, a).
nextStates
  :: (MDP s, Eq s, Ord s)
  => s -> ActionT s -> [(s, Double)]
nextStates s a = combProb . map (first fst) $ jointPMF s a

-- | Rewards and their probabilities - R(s, a, s').
rewards
  :: (MDP s, Eq s)
  => s -> ActionT s -> s -> [(Double, Double)]
rewards s a s' = combProb . map (first snd)
  . filter ((== s') . fst . fst) $ jointPMF s a

-- * Policy Generators
--
-- ^ The following functions take an action-value function, Q(s,a), and
-- produce a policy.

type Policy s a = s -> a
type PolGen s   = MDP s => (s -> ActionT s -> Double)
                        -> Policy s (ActionT s)

-- | Greedy policy.
greedy :: PolGen s
greedy q = \ s -> argmax (q s) (actions s)

-- | Epsilon-Greedy policy.
epsGreedy
  :: (RandomGen g, Eq (ActionT s))
  => g       -- ^ Random number generator.
  -> Double  -- ^ Epsilon; should lie in [0, 1).
  -> PolGen s
epsGreedy gen eps =
  let (x, gen') = random gen
   in if x > eps
        then greedy
        else \q ->
               \s ->
                 let actions' = filter (/= defAct) . actions
                     defAct   = greedy q s
                  in fromMaybe defAct $ head $ shuffle gen' $ actions' s
