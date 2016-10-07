module Data.Statistics.F1 where

f1 :: Foldable f => f Double -> f Double -> Double
f1 truth pred = 2 * (precision' * recall') / (precision' + recall')
  where
    precision', recall' :: Double
    precision' = precision truth pred
    recall' = recall truth pred


precision :: Foldable f => f Double -> f Double -> Double
precision truth pred = undefined

recall :: Foldable f => f Double -> f Double -> Double
recall truth pred = undefined

