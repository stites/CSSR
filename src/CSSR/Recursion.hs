-------------------------------------------------------------------------------
-- | = Phase III: Recursion
--
-- Make epsilon recursively calculable by splitting the states until they have
-- deterministic transitions. In this version of CSSR, we will not remove any
-- transient states since they may prove useful (for things like evolving
-- alphabets).
-------------------------------------------------------------------------------
module CSSR.Recursion where

import CSSR.CausalState.State (State, Events)
import CSSR.Initialization (
    lMax,
    significanceLevel,
    sigma,
    l,
    alphabet,
    parseTree
  )
import CSSR.Sufficiency ( move, test, loop )

-------------------------------------------------------------------------------
-- | recurse
--
-- For each state in allStates take the first history from it and translate it
-- into a state for each possible next-step (using symbols from the alphabet).
--
-- With the same symbol, find the next-step for every other history - if these
-- translations don't match with the first, create a new state with it's own
-- `T(s', b)`(?) and move all matching next-step-predicted histories to that
-- state.
--
-- Repeat until all states have been deterministically split.
-------------------------------------------------------------------------------

recurse :: [State] -> [State]
recurse all@(state:[])  = concat $ map (\ a ->
      split [ a:h | h<-state]
    ) alphabet

recurse all@(state:states)  = recurse [state] ++ recurse states
recurse all = all

-- I think this intrinsically seperates out histories in such a way
-- that we don't need to move them. Testing! that's what we need here.
-- This may also do the same thing that the Transitioning was supposed
-- to do.
split :: [Events] -> [State]
split (h:hs) = h':(split noth)
    where
      h'   = filter ((==) h) hs
      noth = filter ((/=) h) hs

split singleton = [singleton]

