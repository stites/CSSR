package com.typeclassified.hmm.cssr.test.hypothesis

import breeze.linalg.{DenseVector, max}

object KolmogorovSmirnov {
  // TODO: Look up tables for control values and verification
  /** Kolmogorov-Smirnov Hypothesis Test:
    *
    * Given an array `data1[1..n1]`, and an array `data2[1..n2]`, this routine
    * returns the whether or not their K-S statistic passes a hypothesis test
    * with a significance level of `a`. This tells us whether or not the data
    * sets are drawn from the same distribution as determined by the KS
    * distribution.
    *
    * @param data1 the first sample's probability distribution
    * @param n1    the number of observations for data1
    * @param data2 the second sample's probability distribution
    * @param n2    the number of observations for data2
    * @param a     the siginificance level of the test
    * @return      whether or not the pvalue is greater than the significance
    */
  def kstwoTest( data1: DenseVector[Double],
                 n1: Double,
                 data2: DenseVector[Double],
                 n2: Double,
                 a: Double )
  : Boolean = kstwo(data1, n1, data2, n2) > a

  /**
    * Kolmogorov-Smirnov probability (D > observed)
    *
    * This routine returns the significance level for the hypothesis that the
    * data sets are drawn from the same distribution. Small values show that the
    * cumulative distribution function of `data1` is significantly different
    * from that of `data2`.
    *
    * This function is defined as:
    *
    * Probability(D > observed) = Q_{ks}([\sqrt{N_e}+0.12+0.11+\sqrt{N_e}] D])
    *
    * where D is the KS statistic and Q_{ks} is the calculation of significance,
    * defined  by:
    *
    * Q_{ks}(\lambda) = 2 * \sum_{j=1,\infinity}(-1)^{j-1} e^{-2j^{2}\lamda^{2}}
    *
    * and N_e is the effective number of datapoints, defined by:
    *
    * N_e = \div{N_1 * N_2, N_1 + N_2}
    *
    * Note from "Numerical Recipies in C":
    * > The nature of the approximation involved in [Probability(D > observed)]
    * > is that it becomes asymptotically accurate as the N_e becomes large, but
    * > is already quite good for Ne ≥ 4, as small a number as one might ever
    * > actually use.
    *
    * @param data1 the first sample's probability distribution
    * @param n1    the number of observations for data1
    * @param data2 the second sample's probability distribution
    * @param n2    the number of observations for data2
    * @return      p-value, Probability(D > observed)
    */
  def kstwo( data1: DenseVector[Double],
             n1: Double,
             data2: DenseVector[Double],
             n2: Double
          ): Double = {
    val en:Double = math.sqrt(twoSampleEffectiveSize(n1, n2))

    // calculate P(D > observed)
    probks((en + 0.12 + (0.11 / en)) * ksstatistic(data1, data2))
  }

  def twoSampleEffectiveSize(n1:Double, n2:Double) = (n1 * n2) / (n1 + n2)

  /**
    * calculate the Kolmogorov-Smirnov statistic
    *
    * @param data1  sample one's pdf, used to calculate the first ecdf
    * @param data2  sample two's pdf, used to calculate the second ecdf
    * @return       the KS statistic: the supremum of the tow calculated ecdfs
    */
  def ksstatistic(data1: DenseVector[Double],
                  data2: DenseVector[Double]): Double = {
    assert(data1.length == data2.length)

    // calc empirical cumulative distributions. Should have ascending order.
    val ecdf1 = data1.scanRight[Double](0)(_+_)
    val ecdf2 = data2.scanRight[Double](0)(_+_)

    max((ecdf1 - ecdf2).map(math.abs))
  }

  /**
    * Kolmogorov-Smirnov probability function, Q_{ks}
    *
    * Q_{ks}(\lambda) = 2 * \sum_{j=1,\infinity}(-1)^{j-1} e^{-2j^{2}\lamda^{2}}
    *
    * This particular calculation assumes that we are working with discreteness
    * across the natural numbers.
    *
    * @param alam stands for "a lambda", I believe
    * @return     probability or 1.0 if `probks` fails to converge.
    */
  def probks(alam: Double): Double = {
    val EPS1 = 0.001
    val EPS2 = 1.0e-8

    var (term, fac, sum, termBF) :(Double, Double, Double, Double) =
        (0.0,  2.0, 0.0,    0.0)

    val a2 = -2.0 * alam * alam
    for (j <- 1 to 100) {

      term = fac * math.exp(a2 * j * j)
      sum += term
      if ((math.abs(term) <= EPS1 * termBF) || (math.abs(term) <= EPS2 * sum)) {
        return sum
      }
      fac = -fac              // Alternating signs in sum.
      termBF = math.abs(term)
    }

    1.0 // Get here only by failing to converge.
  }
}

