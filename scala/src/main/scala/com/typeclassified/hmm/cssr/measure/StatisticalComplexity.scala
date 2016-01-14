package com.typeclassified.hmm.cssr.measure

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.log
import com.typeclassified.hmm.cssr.state.Machine

// I Think this is the Grassberger-Crutchfield-Young "statistical complexity"
// taking a shot in the dark at what the name might be implying
// relying on anti-cargo-cult paradigms to indicate something is wrong
object StatisticalComplexity {
  def cMu(stateDistribution:DenseVector[Double]) :Double = {
    (-1) * stateDistribution.foldLeft(0d) { (cMu, prob) => cMu + (prob * math.log(prob)) }
  }
}
