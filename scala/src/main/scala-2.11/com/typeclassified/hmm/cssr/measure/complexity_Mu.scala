package com.typeclassified.hmm.cssr.measure

import breeze.linalg.sum
import breeze.numerics.log
import com.typeclassified.hmm.cssr.state.Machine

// I Think this is the Grassberger-Crutchfield-Young "statistical complexity"
// taking a shot in the dark at what the name might be implying
// relying on anti-cargo-cult paradigms to indicate something is wrong
object complexity_Mu {
  def calculate_Complexity_mu(machine:Machine) :Double = {
    return (-1) * sum(machine.distribution :* (log(machine.distribution):/log(2)))
  }
}
