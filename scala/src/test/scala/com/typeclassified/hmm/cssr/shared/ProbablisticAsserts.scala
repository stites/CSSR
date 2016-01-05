package com.typeclassified.hmm.cssr.shared

import org.scalatest.Matchers

trait ProbablisticAsserts extends Matchers {
  def assertProbabalisticDetails(probablistic: Probablistic, freqs: Array[Double]):Unit = {
    probablistic.totalCounts          should be (freqs.sum)
    probablistic.frequency.toArray    should contain theSameElementsInOrderAs freqs
    probablistic.distribution.toArray should contain theSameElementsInOrderAs freqs.map(f => if (f == 0) 0 else f/(freqs.sum))
  }
}
