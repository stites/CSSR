module Data.Statistics.KologorovSmirnov where

import Data.Function

---------------------------------------------------------------------------------
-- | Kolmogorov-Smirnov Hypothesis Test:
--
-- Given an array @data1[1..n1]@, and an array @data2[1..n2]@, this routine
-- returns the whether or not their K-S statistic passes a hypothesis test
-- with a significance level of @a@. This tells us whether or not the data
-- sets are drawn from the same distribution as determined by the KS
-- distribution.
--
-- TODO: Look up tables for control values and verification
---------------------------------------------------------------------------------
kstwoTest ::
  [Double] -- ^the first sample's probability distribution
  -> Double -- ^the number of observations for data1
  -> [Double] -- ^the second sample's probability distribution
  -> Double -- ^the number of observations for data2
  -> Double -- ^the siginificance level of the test
  -> Bool -- ^whether or not the pvalue is greater than the significance
kstwoTest data1 n1 data2 n2 a = kstwo data1 n1 data2 n2 > a

---------------------------------------------------------------------------------
-- | Kolmogorov-Smirnov probability (D > observed)
--
-- This routine returns the significance level for the hypothesis that the
-- data sets are drawn from the same distribution. Small values show that the
-- cumulative distribution function of @data1@ is significantly different
-- from that of @data2@.
--
-- This function is defined as:
--
-- Probability(D > observed) = Q_{ks}([\sqrt{N_e}+0.12+0.11+\sqrt{N_e}] D])
--
-- where D is the KS statistic and Q_{ks} is the calculation of significance,
-- defined  by:
--
-- Q_{ks}(\lambda) = 2 * \sum_{j=1,\infinity}(-1)^{j-1} e^{-2j^{2}\lamda^{2}}
--
-- and N_e is the effective number of datapoints, defined by:
--
-- N_e = \div{N_1 * N_2, N_1 + N_2}
--
-- Note from "Numerical Recipies in C":
-- > The nature of the approximation involved in [Probability(D > observed)]
-- > is that it becomes asymptotically accurate as the N_e becomes large, but
-- > is already quite good for Ne ≥ 4, as small a number as one might ever
-- > actually use.
--
---------------------------------------------------------------------------------

kstwo ::
  [Double] -- ^the first sample's probability distribution
  -> Double -- ^the number of observations for data1
  -> [Double]  -- ^the second sample's probability distribution
  -> Double -- ^the number of observations for data2
  -> Double -- ^p-value, Probability(D > observed)
kstwo data1 n1 data2 n2 = probks $ ksstatistic data1 data2 * (en + 0.12 + (0.11 / en))
  where
    en :: Double
    en = sqrt effectSize

    -- the two-sample effective size
    effectSize :: Double
    effectSize = (n1 * n2) / (n1 + n2)

---------------------------------------------------------------------------------
-- the Kolmogorov-Smirnov statistic
---------------------------------------------------------------------------------
ksstatistic ::
  [Double] -- ^sample one's pdf, used to calculate the first ecdf
  -> [Double] -- ^sample two's pdf, used to calculate the second ecdf
  -> Double -- ^the KS statistic: the supremum of the tow calculated ecdfs
ksstatistic data1 data2 =
    -- assert(data1.length == data2.length)
    foldr1 max $ map abs $ getAll data1 data2
  where
    -- | calc empirical cumulative distributions. Should have ascending order.
    ecdf :: [Double] -> [Double]
    ecdf = scanr (+) 0

    getAll :: [Double] -> [Double] -> [Double]
    getAll d1 d2 = zipWith subtract (ecdf d1) (ecdf d2)

---------------------------------------------------------------------------------
-- | Kolmogorov-Smirnov probability function, Q_{ks}
--
-- Q_{ks}(\lambda) = 2 * \sum_{j=1,\infinity}(-1)^{j-1} e^{-2j^{2}\lamda^{2}}
--
-- This particular calculation assumes that we are working with discreteness
-- across the natural numbers.
---------------------------------------------------------------------------------
probks :: Double -> Double -- ^probability or 1.0 if @probks@ fails to converge.
probks alam {-alam stands for "a lambda", I believe-}= go [1..100] 2 0 0
  where
    a2, eps1, eps2 :: Double
    a2 = -2 * (alam ** 2)
    eps1 = 0.001
    eps2 = 1.0e-8

    go :: [Double] -> Double -> Double -> Double -> Double
    go    []  fac oldsum termBF = 1  -- Get here only by failing to converge.
    go (j:js) fac oldsum termBF =
      let
        aterm, term, newsum :: Double
        term = fac * exp (a2 * j * j)
        newsum = oldsum + term
        aterm = abs term
      in
        if (aterm <= eps1 * termBF) || (aterm  <= eps2 * newsum)
        then newsum
        else go js (-1 * fac) newsum aterm

