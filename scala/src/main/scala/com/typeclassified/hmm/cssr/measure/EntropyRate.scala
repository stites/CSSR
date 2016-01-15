package com.typeclassified.hmm.cssr.measure

import com.typeclassified.hmm.cssr.state.Machine

object EntropyRate {

  def entropyRate(machine:Machine):Double = {
    (-1) * machine.states
      .view
      .zipWithIndex
      .map {
        case (state, i) =>
          val freq = machine.distribution(i)

          state.distribution.foldLeft(0d) {
            // technically, we can remove branching, but I don't know what scala will do, given log(0)
            case (stateEntRate, prob) if prob > 0 => stateEntRate + (freq * (prob * math.log(prob)))
            // Also note that scala logs are the natural log
            case (         ser, prob) if prob <=0 => ser
          } }
      .sum[Double]
  }

}
