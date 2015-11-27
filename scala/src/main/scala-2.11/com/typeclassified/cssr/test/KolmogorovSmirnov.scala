package com.typeclassified.cssr.test

import breeze.linalg.{DenseVector, max}
import breeze.numerics.abs

object KolmogorovSmirnov {
  // TODO: Look up tables for control values and verification
  def test(dist1: DenseVector[Double],
           dist1Total: Double,
           dist2: DenseVector[Double],
           dist2Total: Double
          ): Double = {
    assert(dist1.length == dist2.length)

    // obtain cumulative distributions
    val cdf1 = dist1.scanRight(0d)(_ + _)
    val cdf2 = dist2.scanRight(0d)(_ + _)

    // calculate KS statistic - take max difference between 2 values
    val largestDiff: Double = max(abs(cdf1 - cdf2))

    val x: Double = (dist1Total * dist2Total) / (dist1Total + dist2Total)
    val en: Double = math.sqrt(x)

    // calculate significance level
    prob((en + 0.12 + (0.11 / en)) * largestDiff)
  }

  def prob(alam: Double): Double = {
    val a2 = -2.0 * alam * alam
    val EPS1 = 0.001
    val EPS2 = 1.0e-8

    var term = 0.0
    var fac = 2.0
    var acc = 0.0
    var termBF = 0.0

    for (j <- 1 to 100) {
      term = fac * math.exp(a2 * j * j)
      acc += term
      if ((math.abs(term) <= EPS1 * termBF) || (math.abs(term) <= EPS2 * acc)) {
        return acc
      }
      fac = -fac
      termBF = math.abs(term)
    }

    1.0
  }
}
