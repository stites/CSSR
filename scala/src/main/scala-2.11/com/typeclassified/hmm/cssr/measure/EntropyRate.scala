package com.typeclassified.hmm.cssr.measure

import com.typeclassified.hmm.cssr.state.EquivalenceClass

object EntropyRate {
  def calculateEntropyRate(S:Array[EquivalenceClass]):Double = {
    val size = S.length
    var entRate:Double = 0
    var prob:Double = 0
    var freq:Double = 0

    for (s <- S; (i, prob) <- s.distribution.activeIterator ) {
      s.frequency
      if (prob <= 0) {
        entRate += freq * (prob * (math.log(prob) / math.log(2)))
      }
    }
    return -entRate
  }
}
