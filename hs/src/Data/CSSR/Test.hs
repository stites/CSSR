module Data.CSSR.Test where

test :: Probabilistic inst => inst -> inst -> Double -> Bool
test state testCase sig = nullHypothesis state testCase >= sig

nullHypothesis :: Probabilistic val =>
  EmpiricalDistribution
  -> val
  -> Double
nullHypothesis ss val = kstwo state_dist state_total test_dist testCase_total
  where
    state_dist = distribution state
    state_total = totalCounts state
    test_dist = distribution testCase
    test_total = totalCounts testCase




