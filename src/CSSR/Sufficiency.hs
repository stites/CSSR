-------------------------------------------------------------------------------
-- | = Phase II: Sufficiency
--
-- "Phase II iteratively tests the successive versions of the null hypothesis"
-- ...but in haskell we'll do this recursively : )
-------------------------------------------------------------------------------
module CSSR.Sufficiency where

import CSSR.CausalState.State (probability, State)
import CSSR.CausalState.History (Moment, History)
import CSSR.Initialization (
    lMax,              -- | default = 5
    significanceLevel, -- | default = 0.05
    sigma,             -- | is      = []
    l,                 -- | is      = 0
    alphabet,          -- | given
    parseTree          -- | given
  )

---------------------------------------------------------------------------
-- | loop
--
-- Will iterate through the ParseTree
-- in the causal state. If it does not, it will perform a restricted
-- hypothesis test to see if the history belongs in any existing state.
--
-- If neither test succeeds, we will generate a new state for this history.
-- ========================================================================
-- TODO: TCO at a later point
---------------------------------------------------------------------------
loop :: [State] -> Integer -> [State]
loop all@(s:[])     l | l < lMax = loop (test all (constructedHistory) s alpha) (l+1)
loop all@(s:states) l | l < lMax = loop (test all (constructedHistory) (expandStates) alpha) (l+1)

loop all _ = all -- for everything else

---------------------------------------------------------------------------
-- | test
--
-- Will take a history and perform a hypothesis test to see if it belongs
-- in the causal state. If it does not, it will perform a restricted
-- hypothesis test to see if the history belongs in any existing state.
--
-- If neither test succeeds, we will generate a new state for this history.
---------------------------------------------------------------------------
test :: Fractional p => [State] -> History -> State -> Float -> [State]
test allStates history state alpha = let
    nullHypothesis = hypothesisTest state
    someState = find hypothesisTest allStates
  in
    if nullHypothesis
    then state
    else if (isJust someState)
         then (moveStatesTo state') ++ allStates
         else (moveStatesTo []    ) ++ allStates
  where
    moveStatesTo state'= move history state state'
    hypothesisTest inState = (probability history inState) < (1 - alpha)

---------------------------------------------------------------------------
-- | move
--
-- takes two states and a history and moves it from the first state to the
-- second.
-- ========================================================================
-- TODO: re-estimation doesn't seem to fit here - could it be redundant?
---------------------------------------------------------------------------
move :: History -> State -> State -> [State, State]
move x s1 s2 = [s1', s2']
  where
    s1' = filter ((/=) x) s1
    s2' = if (x `elem` s2) then s2 else x:s2


