package com.typeclassified.hmm.cssr.measure

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.log
import com.typeclassified.hmm.cssr.parse.Alphabet
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
            (stateEntRate:Double, prob:Double) => {
              // technically, we can remove branching, but I don't know what scala will do, given log(0)
              if (prob <= 0) stateEntRate else stateEntRate + (freq * (prob * math.log(prob)))
              // Also note that scala logs are the natural log
            }
          } }
      .sum[Double]
  }

}
