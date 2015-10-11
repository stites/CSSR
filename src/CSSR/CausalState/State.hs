-----------------------------------------------------------------------
-- |
-- CSSR.CausalState.State:
-- A casaul state's State monad represents the collection of Histories
-- that make up a causal state
--
-----------------------------------------------------------------------
module CSSR.CausalState.State (State, probability) where

import CSSR.CausalState.History (History)

-- TODO: these are just a bunch of alias' right now - It may be cleaner
-- to have the State be a monad
type State = [History]

probability :: Fractional p => State -> History -> p
probability state x = counted / stateSize
  where
    divisibleLength = realToFrac.length
    counted         = divisibleLength (filter ((==) x) state)
    stateSize       = divisibleLength state

