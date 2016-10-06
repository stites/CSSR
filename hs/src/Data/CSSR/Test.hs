module Data.CSSR.Test where

test :: Probabilistic inst => inst -> inst -> Double -> Bool
test state testCase sig = (nullHypothesis state testCase) >= sig

nullHypothesis :: Probabilistic val =>
  EmpiricalDistribution
  -> val
  -> Double
nullHypothesis ss val =
  kstwo (distribution state) (totalCounts state) (distribution testCase) (totalCounts testCase)
  where




