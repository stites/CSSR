package com.typeclassified.hmm.cssr.test

import breeze.linalg.{max, sum, DenseVector}
import com.typeclassified.hmm.cssr.test.hypothesis.{KolmogorovSmirnov=>KS}
import org.scalactic.TolerantNumerics
import org.scalatest.{FlatSpec, Matchers}

class KolmogorovSmirnovTest extends FlatSpec with Matchers {
  val sig:Double = 0.05
  val epsilon:Double = 1e-4f

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  behavior of "kstwoTest"

  it should "accept the test, given two identical distributions" in {
    val freq: DenseVector[Double] = new DenseVector(Array(2d, 5d, 8d))
    val count = sum(freq)
    val probs: DenseVector[Double] = freq :/ count
    KS.kstwoTest(freq, count, freq, count, sig) should be (true)
  }

  it should "reject the test, given inverse distributions" in {
    val support = Array(0d, 3d, 9d)
    val count = support.sum
    KS.kstwoTest(
      new DenseVector(support) :/ count,
      count,
      new DenseVector(support.reverse) :/ count,
      count,
      sig) should be (false)
  }

  "KolmogorovSmirnov package under two pre-validated scenarios" should "return expected results" in {
    // from http://www.real-statistics.com/non-parametric-tests/two-sample-kolmogorov-smirnov-test/

    val m = new DenseVector[Double](Array(4,11,5,7,0,5,9,13,20,6).map{_.toDouble})
    val w = new DenseVector[Double](Array(7,4,1,11,12,4,2,4,8,9).map{_.toDouble})
    val (mPDF, wPDF) = (m / sum(m), w / sum(w))
    val (mwGivenKSStat, mwGivenPVal) = (0.229032, 0.043055)
    val (a, b) = (KS.ksstatistic(mPDF, wPDF), KS.kstwo(mPDF, sum(m), wPDF, sum(w)))

    assert(KS.ksstatistic(mPDF, wPDF) === mwGivenKSStat)
    assert(KS.kstwo(mPDF, sum(m), wPDF, sum(w)) === mwGivenPVal)
    KS.kstwoTest(mPDF, sum(m), wPDF, sum(w), sig) should be (false)

    val i = new DenseVector[Double](Array(1,2,0,1,0,3,1).map{_.toDouble})
    val f = new DenseVector[Double](Array(2,1,2,0,1,1,0).map{_.toDouble})
    val (iPDF, fPDF) = (i / sum(i), f / sum(f))
    val (ifGivenKSStat, ifGivenPVal) = (0.357143, 0.62169)

    assert(KS.ksstatistic(iPDF, fPDF) === ifGivenKSStat)
    assert(KS.kstwo(iPDF, sum(i), fPDF, sum(f)) === ifGivenPVal)
    KS.kstwoTest(iPDF, sum(i), fPDF, sum(f), sig) should be (true)
  }
}
