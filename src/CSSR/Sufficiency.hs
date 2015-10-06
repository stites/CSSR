module CSSR.Sufficiency where

import CSSR.Initialization

{-
loop l lMax sigma = if l < lMax
                    then
                      loopSigma l sigma -- do estimates across sigmas
                      loop (l + 1) lMax sigma -- loop
                    else return ()
loopSigma l (s:sigmas) = estimatePHat s
loopSigma l (s:[])     = estimatePHat s
loopSigma l _          = return ()
testState states probability alphabetCharTimesIncrement state sigLevel =
  if testNullHypotheses sigLevel
-}

