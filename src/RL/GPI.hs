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

import qualified Data.Vector.Sized   as VS
import Data.Vector.Sized             (Vector)

import Control.Monad.Writer
import Data.Finite
import Data.List                     (lookup, groupBy, unzip5)
import Data.List.Extras.Argmax       (argmax)
import Data.MemoTrie
import System.Random
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
  , nSteps     :: Int     -- ^ # of steps in n-step TD
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
  , nSteps     = 0     -- TD(0) by default.
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
  :: forall s.
     ( MDP s, Ord s, Eq (ActionT s)
     , HasFin' s, HasFin' (ActionT s)
     -- , KnownNat (Card s), KnownNat (Card (ActionT s))
     )  -- , Ord r )  -- , Eq s, Eq a)
  => HypParams (Double)  -- ^ Simulation hyper-parameters.
  -> Int                 -- ^ Number of episodes to run.
  -> TDRetT s (ActionT s) (Double)
doTD hParams@HypParams{..} nIters = TDRetT vs ps pDiffs vErrs dbgss where
  gen = mkStdGen 1
  (qs, dbgss) = unzip $ flip evalState (VS.replicate (VS.replicate 0), gen, 0) $
    replicateM nIters $ do
      (q, g, t) <- get
      let s = case initStates @s of
                [] -> P.head states
                _  -> P.head $ shuffle g initStates
          -- (_, q', g', dbgs, t') = optQn hParams (s, q, g, [], t)
          (_, q's, g's, dbgs, t's) =
            unzip5 $ take maxIter $ P.tail
                   $ iterate (optQn hParams) (s, q, g, [], t)
          q' = P.last q's
      put (q', P.last g's, P.last t's)
      return (q', P.last dbgs)
  -- Calculate policies and count differences between adjacent pairs.
  ps     = map (\q -> \s -> argmax (appFm q s) $ actions s) qs
  ass    = map (\p -> map p states) ps
  pDiffs = map (length . filter (== False)) $ zipWith (zipWith (==)) ass (P.tail ass)
  -- Calculate value functions and mean square differences between adjacent pairs.
  vs     = map (\q -> \s -> maximum . map (appFm q s) $ actions s) qs
  valss  = map (\v -> map v states) vs
  vErrs  = map (mean . map sqr) $ zipWith (zipWith (-)) valss (P.tail valss)

-- | Yields a single episodic action-value improvement iteration, using n-step TD.
--
-- HypParams field overrides:
-- - epsilon: Used to form an "epsilon-greedy" policy.
-- - maxIter: The "n" in n-step.
optQn
  :: ( MDP s, HasFin' s, Ord s
     , Eq (ActionT s), HasFin' (ActionT s)
     , RandomGen g
     )
  => HypParams (Double)  -- ^ Simulation hyper-parameters.
  -> (s, Vector (Card s) (Vector (Card (ActionT s)) (Double)), g, [Dbg s (Double)], Integer)
  -> (s, Vector (Card s) (Vector (Card (ActionT s)) (Double)), g, [Dbg s (Double)], Integer)
optQn HypParams{..} (s0, q, gen, _, t) =
  (P.last sNs, q', gen', debgs, t + fromIntegral nSteps + 1)
 where
  (sNs, _, q', gen', debgs, _) =
    execState (traverse go [(-nSteps)..(max 0 (nSteps - 1))])
              ([s0], [], q, gen, [], [])
  gammas = scanl (*) 1 $ repeat disc  -- [1, disc, disc^2, ...]
  go = \ n -> do  -- Single state transition with potential update to Q.
    (ss, as, qm, g, dbgs, rs) <- get
    let -- Helpful abbreviations
        qf        = appFm qm   -- From matrix representation of Q(s,a) to actual function.
        s         = P.last ss  -- This is NOT the state to be updated (unless we're doing TD(0)).
        alpha'    = alpha   / decayFact
        epsilon'  = epsilon / decayFact
        decayFact = (1 + beta * fromIntegral t)

        -- Policy definitions
        (x, g') = random g
        pol' st = argmax (qf st) (actions st)  -- greedy
        pol  st = if x > epsilon'              -- epsilon-greedy
                    then pol' st
                    else case otherActs of
                           [] -> pol' st
                           xs -> P.head $ shuffle gen $ xs
         where otherActs = filter (/= (pol' st)) $ actions st

        -- Use epsilon-greedy policy to choose next action.
        a   = pol s
        as' = as ++ [a]

        -- Produce a sample next state, obeying the actual distribution.
        s' = P.head $ shuffle gen' $
               concat [ replicate (round $ 100 * p) st
                      | (st, p) <- s'p's
                      ]
        s'p's  = nextStates s a

        -- Calculate reward and Q(s',a').
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

        -- Only begin updating Q after `nSteps` state transitions.
        qm' = if n >= 0
                then let sN     = ss  P.!! n
                         aN     = as' P.!! n
                         sNum   = toFin sN
                         aNum   = toFin aN
                         newVal =
                           fromMaybe
                             (oldVal
                              + alpha'
                                * ( ( sum $
                                        zipWith (*)
                                                (r : (take nSteps $ drop n rs) ++ [eQs'a'])
                                                gammas
                                    ) - oldVal )
                             ) $ lookup s termStates
                         oldVal = qf sN aN
                      in qm VS.// [ ( sNum
                                    , qm `VS.index` sNum VS.// [(aNum, newVal)]
                                    )
                                  ]
                else qm

        -- TEMPORARY DEBUGGING INFO
        dbg   = Dbg s'p's s s' r eQs'a' $
                  [ [ qf st act
                    | act <- actions st
                    ]
                  | (st, _) <- termStates
                  ]

    put (ss ++ [s'], as', qm', g', dbgs ++ [dbg], rs ++ [r])

-- | Yields a single policy improvment iteration.
--
-- RLType field overrides:
-- - maxIter: max. policy evaluation iterations (0 = Value Iteration)
--
-- Returns a combined policy & value function.
optPol :: ( MDP s, Eq s, Ord s, HasTrie s
          , Ord (ActionT s), HasTrie (ActionT s)
          -- , KnownNat m, KnownNat n
          )
       => HypParams Double      -- ^ Simulation hyper-parameters.
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
      ) $ lookup s termStates
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

