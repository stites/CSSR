package com.typeclassified.hmm.cssr

import org.scalatest.{FlatSpec, Matchers, BeforeAndAfter}

trait ProbablisticAsserts extends Matchers {
  def assertProbabalisticDetails(probablistic: Probablistic, total:Double, freqs:Array[Double]):Unit = {
    probablistic.totalCounts          should be (total)
    probablistic.frequency.toArray    should contain theSameElementsInOrderAs freqs
    probablistic.distribution.toArray should contain theSameElementsInOrderAs freqs.map(f => if (f == 0) 0 else f/total)
  }
}
