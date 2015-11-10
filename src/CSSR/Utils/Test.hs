module CSSR.Utils.Test where

nullHypothesis :: [String] -> String -> Rational
nullHypothesis states observed = let
    total = filterByLength states
    count = filterHistories total
  in
    realLen total / realLen count
  where
    filterByLength  = filter (\ s -> length s == (length.tail) observed)
    filterHistories = filter ((==) $ tail observed)
    realLen = realToFrac.length

