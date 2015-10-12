-------------------------------------------------------------------------------
-- | = Phase II: Sufficiency
--
-- "Phase II iteratively tests the successive versions of the null hypothesis"
-- ...but in haskell we'll do this recursively : )
-------------------------------------------------------------------------------
module CSSR.Sufficiency where

import Data.List (find)
import Data.Maybe (isJust, fromJust)
import CSSR.CausalState.State (probability, State)
import CSSR.CausalState.History (Moment, History)
import CSSR.Initialization (
    lMax,              -- | default = 5
    significanceLevel, -- | default = 0.05
    sigma,             -- | is      = [""]
    l,                 -- | is      = 0
    alphabet,          -- | given
    parseTree          -- | given
  )

---------------------------------------------------------------------------
-- | loop
--
-- Will iterate through the states in a breadth-first manner. As we pass
-- through each tree node, we construct new states by prefixing characters
-- from the given alphabet.
-- ========================================================================
-- TODO: TCO at a later point
---------------------------------------------------------------------------
loop :: [State] -> Int -> [State]
loop all@(state:_) l | l < lMax = let histories = constructFrom state
--  [['']]                            [['a'],['b'],['c']]
  in concat [loop (test all history state alpha) (l+1) | history <- histories]

loop all _ = all -- for everything else

constructFrom :: State -> [History]
constructFrom state = [ a:h | a<-alphabet, h<-state]

alpha = significanceLevel -- ^ shorthand
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
    then allStates -- ^ if the null hypothesis is true, keep as-is
    else if (isJust someState)
         then (moveStatesTo $ fromJust someState) ++ allStates
         else (moveStatesTo                 []) ++ allStates
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
move :: History -> State -> State -> [State]
move x s1 s2 = [s1', s2']
  where
    s1' = filter ((/=) x) s1
    s2' = if (x `elem` s2) then s2 else x:s2


