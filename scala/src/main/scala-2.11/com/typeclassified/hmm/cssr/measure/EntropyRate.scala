package com.typeclassified.hmm.cssr.measure

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.log
import com.typeclassified.hmm.cssr.state.Machine

object EntropyRate {
  def calculateEntropyRate(machine:Machine):Double = {
    // replacing with a map would seem logical -- but I seem to be hitting an "implicit" roadblock...
    // ...if you know what I mean ; D
    var entRate:Double = 0
    for ((s, i) <- machine.states.view.zipWithIndex ) {
      val probComponent:DenseVector[Double] = s.distribution :* log(s.distribution) :/ math.log(2)
      entRate += sum(machine.frequency(i) * probComponent)
    }
    return -entRate
  }
}
