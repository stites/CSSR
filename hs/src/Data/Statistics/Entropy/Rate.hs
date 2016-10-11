module Data.Statistics.Entropy.Rate where

import Data.Function

type Probability = Double
type Frequency = Integer
type ProbabilityDistribution = [Probability]
type FrequencyDistribution = [Frequency]

{-
entropyRate :: [(ProbabilityDistribution, FrequencyDistribution)] -> Double
entropyRate = ((-1) *) . sum . map stateER
  where
    stateER :: (ProbabilityDistribution, FrequencyDistribution) -> Double
    stateER (ps, fs) = foldr go 0 (zip ps fs)

    go :: Double -> (Probability, Frequency) -> Double
    go entr (p, f) = if p <= 0 then entr else entr + f * p * log p

discreteEntropy :: Double -> Double -> Double
discreteEntropy a b = a * log (a / b)

data ParseLeaf
  = ParseLeaf
  { totalCounts :: Integer
  } deriving (Show)

type InferredDistribution = [(ParseLeaf, Double)]

-- |
-- calculates the probability of all the max length strings in the data based on
-- the inferred machine.
relativeEntropy
  :: InferredDistribution -- ^ dist the inferred distribution of the *histories of max length*.
  -> Integer -- ^ data size
  -> Double
relativeEntropy dist dataSize = undefined -- if relEnt < 0 then 0 else relEnt
  where
    inferredPs = map snd dist
    observedPs = map (nonZero . toP . totalCounts . fst) dist

    toP :: Integer -> Probability
    toP counts = ((/) `on` fromIntegral) counts dataSize

    nonZero :: Double -> Double
    nonZero p = if p < 0 then 0 else p -- paranoia from original C version

    go :: Double -> (ParseLeaf, Double) -> Double
    go ent (leaf, infP) =
      let
        p = undefined -- obsP leaf
      in
        ent + (if p > 0 then discreteEntropy p infP else 0)

--
-- Kullback-Leibler Distance:
--
-- \begin{equation}
--    d= \sum_{k} p_k * \log_2 * { p_k \over q_k }
-- \end{equation}
--
klDist
  :: [Probability] -- ^ observed probabilities
  -> [Probability] -- ^ inferred probabilities
  -> Double
klDist = sum (uncurry . discreteEntropy) . zip

-}
