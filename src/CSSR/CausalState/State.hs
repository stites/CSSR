-----------------------------------------------------------------------
-- |
-- CSSR.CausalState.State:
-- A casaul state's State monad represents the collection of Histories
-- that make up a causal state
--
-----------------------------------------------------------------------
module CSSR.CausalState.State where

-- TODO: these are just a bunch of alias' right now - It may be cleaner
-- to have the State be a monad

type Event  = Char -- TODO: can also be ints... basically whatever the alphabet is
type Events = [Event]
type State  = [Events]

probability :: Fractional p => Events -> State -> p
probability x state = counted / stateSize
  where
    divisibleLength = realToFrac.length
    counted         = divisibleLength (filter ((==) x) state)
    stateSize       = divisibleLength state

