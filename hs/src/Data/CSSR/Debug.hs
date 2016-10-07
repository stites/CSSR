module Data.CSSR.Debug where

hypothesisTesting testCase states =
  intercalate "\n"
  [ "Testing: " ++ show testCase
  , "Have state information:"
  , "======================================"
  , "state.histories.foreach{ h => debug(h.toString) }"
  , "======================================"
  , "Running test -- Count1: " ++ show (totalCounts state) ++ " Count2: " ++ show (totalCounts state)
  , "state: ${state.distribution.toString}\t\tfreq1: ${state.frequency.toString()}"
  , " leaf: ${testCase.distribution.toString}\t\tfreq2: ${testCase.frequency.toString()}"
  ]


