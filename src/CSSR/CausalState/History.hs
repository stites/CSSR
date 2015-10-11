-----------------------------------------------------------------------
-- |
-- CSSR.CausalState.History:
-- A monad which represents a causal state's history
--
-----------------------------------------------------------------------
module CSSR.CausalState.History where

type Moment  = Char
type History = [Moment]

-- TODO: I feel like one of these would really clean this up
-- instance Monad History where
--   (>>=)     = concatMap
--   return a  = [a]
--   fail _    = []

