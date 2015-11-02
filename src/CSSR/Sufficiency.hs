-------------------------------------------------------------------------------
-- | = Phase II: Sufficiency
--
-- "Phase II iteratively tests the successive versions of the null hypothesis"
-- ...but in haskell we'll do this recursively : )
-------------------------------------------------------------------------------
module CSSR.Sufficiency where

import qualified CSSR.Parse.Tree as PT

-- TODO: REMOVE THIS
import Debug.Trace

import Data.List (find)
import Data.Maybe (isJust, fromJust)
import CSSR.CausalState.State (probability, State, Events)
import CSSR.Initialization (
    lMax,              -- | default = 5
    significanceLevel, -- | default = 0.05
    sigma,             -- | is      = [""]
    l,                 -- | is      = 0
    alphabet,          -- | given
    parseTree          -- | given
  )

-------------------------------------------------------------------------------
-- | loop
--
-- Will iterate through the parseTree in a breadth-first manner. As we pass
-- through each tree node, we construct new states by prefixing characters
-- from the given alphabet.
-- ============================================================================
-- TODO: TCO at a later point
-------------------------------------------------------------------------------
loop :: [State] -> Int -> [State]
loop all@(state:_) l | l < lMax = concat $
    [loop (test all observedHistory newState alpha) (l+1) | observedHistory <- observedState ]
  where
    newState = constructFrom state
    observedState::[Events]
    observedState = PT.walk (PT.getBranches parseTree) l

loop all _ = all -- for everything else

alpha :: Float
alpha = significanceLevel -- ^ shorthand

constructFrom :: State -> [Events]
constructFrom state = [ a:h | a<-alphabet, h<-state]

-------------------------------------------------------------------------------
-- | test
--
-- Will take a history and perform a hypothesis test to see if it belongs
-- in the causal state. If it does not, it will perform a restricted
-- hypothesis test to see if the history belongs in any existing state.
--
-- If neither test succeeds, we will generate a new state for this history.
-------------------------------------------------------------------------------
test :: Fractional p => [State] -> Events -> State -> Float -> [State]
test allStates history state alpha = let
    nullHypothesis = hypothesisTest state
    someState = find hypothesisTest allStates
  in
    if (trace "in test" nullHypothesis)
    then allStates -- ^ if the null hypothesis is true, keep as-is
    else if (isJust someState)
         then (moveStatesTo $ fromJust someState) ++ allStates
         else (moveStatesTo                 []) ++ allStates
  where
    moveStatesTo state'= move history state state'
    hypothesisTest inState = (probability history inState) < (1 - alpha)

-------------------------------------------------------------------------------
-- | move
--
-- takes two states and a history and moves it from the first state to the
-- second.
-- ============================================================================
-- TODO: re-estimation doesn't seem to fit here - could it be redundant?
-------------------------------------------------------------------------------
move :: Events -> State -> State -> [State]
move x s1 s2 = [s1', s2']
  where
    s1' = filter ((/=) x) s1
    -- | TODO:
    -- WARNING, this will lessen redundancy and, perhaps, make probabilities
    -- incorrect. verify.
    s2' = if (x `elem` s2) then s2 else x:s2


