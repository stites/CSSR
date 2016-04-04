package com.typeclassified.hmm.cssr.shared

import breeze.linalg.DenseVector
import breeze.numerics._
import org.scalatest.Matchers

trait ProbablisticAsserts extends Matchers {
  implicit val ep:Epsilon = new Epsilon(0.01)

  def assertApproximateDistribution(dist:DenseVector[Double], probs: Array[Double])(implicit ep:Epsilon):Unit = {
    abs(probs.sum - 1) should be < ep.precision
    (abs(dist :- new DenseVector(probs)) :< ep.precision).reduceRight(_&&_) should be (true)
  }

  def assertProbabalisticDetails(probablistic: Probablistic, freqs: Array[Double]):Unit = {
    probablistic.totalCounts          should be (freqs.sum)
    probablistic.frequency.toArray    should contain theSameElementsInOrderAs freqs
    probablistic.distribution.toArray should contain theSameElementsInOrderAs freqs.map(f => if (f == 0) 0 else f/freqs.sum)
  }
}
