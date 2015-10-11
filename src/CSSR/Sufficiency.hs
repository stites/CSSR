module CSSR.Sufficiency where

import CSSR.CausalState.State (probability, State)
import CSSR.CausalState.History (Moment, History)
import CSSR.Initialization (
    lMax,              -- default = 5
    significanceLevel, -- default = 0.05
    sigma,             -- is      = []
    l,                 -- is      = 0
    alphabet,          -- given
    parseTree          -- given
  )

-- -- TODO: TCO at a later point
-- loop :: [State] -> Integer -> [State]
-- loop allStates@(s:[])     l | l < lMax = loop
--                                        $ test allStates (proba
-- loop allStates@(s:states) l | l < lMax = loop
--                                        $ test allStates (proba
--
-- -- for everything else (should just be the l's):
-- loop allStates _ = allStates

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

-- TODO: I've removed re-estimation. Is it redundant?
move :: History -> State -> State -> [State, State]
move x s1 s2 = [s1', s2']
  where
    s1' = filter ((/=) x) s1
    s2' = if (x `elem` s2) then s2 else x:s2


