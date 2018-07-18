------------------------------------------------------------------------
-- |
-- Module      :  RL.MDP
-- Description :  Markov Decision Process
-- Copyright   :  (c) Target Corp., 2018
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David.Banas@target.com
-- Stability   :  experimental
-- Portability :  ?
--
-- Markov Decision Process (MDP)
------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

module RL.MDP where

import qualified Prelude as P
import Protolude  hiding (show, for)

import Control.Arrow                 ((&&&))
import Data.List                     (groupBy)

-- | Markov Decision Process (MDP) (finite discrete)
--
-- Laws:
--
-- 1. @jointPMF@ must return a true probability distribution. That is:
--
--     @
--     1 = sum . map snd $
--           [ jointPMF s a
--           | s <- states
--           , a <- actions s
--           ]
--     @
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
class MDP s where
  type ActionT s :: *
  -- | State enumeration function - \(S\).
  states :: [s]
  -- | Action enumeration function - \(A(s)\).
  actions :: s -> [ActionT s]
  -- | Joint probability distribution - \(Pr[(s', r) | s, a]\).
  jointPMF :: s -> ActionT s -> [((s, Double), Double)]
  -- | Initial states.
  initStates :: [s]
  initStates = states
  -- | Terminal states and their values.
  termStates :: [(s, Double)]
  termStates = []

{----------------------------------------------------------------------
  Helper functions applicable only to MDPs.
----------------------------------------------------------------------}

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

{----------------------------------------------------------------------
  Helper functions expected to be used only locally.
----------------------------------------------------------------------}

-- | Eliminate duplicates from a probability distribution, by combining
-- like terms and summing their probabilities.
combProb
  :: (Eq a, Ord a)
  => [(a, Double)] -> [(a, Double)]
combProb =
  map ((fst . P.head) &&& (sum . map snd))
  . groupBy ((==)    `on` fst)
  . sortBy  (compare `on` fst)

