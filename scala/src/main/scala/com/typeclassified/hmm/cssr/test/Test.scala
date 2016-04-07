package com.typeclassified.hmm.cssr.test

import com.typeclassified.hmm.cssr.shared.{EmpiricalDistribution, Probablistic, Level, Logging}
import com.typeclassified.hmm.cssr.test.hypothesis.{KolmogorovSmirnov=>KS}

object Test extends Logging {
  override def loglevel() = Level.OFF

  def nullHypothesis(state: EmpiricalDistribution, testCase: Probablistic, sig:Double): Double = {
    debug("Testing: " + testCase.toString)
    debug(s"Have state information:")
    debug("======================================")
    state.histories.foreach{ h => debug(h.toString) }
    debug("======================================")
    debug(s"Running test -- Count1: ${state.totalCounts} Count2: ${testCase.totalCounts}")
    debug(s"state: ${state.distribution.toString}\t\tfreq1: ${state.frequency.toString()}")
    debug(s" leaf: ${testCase.distribution.toString}\t\tfreq2: ${testCase.frequency.toString()}")
    nullHypothesis(state, testCase, sig)
  }

  def nullHypothesis(state: Probablistic, testCase: Probablistic, sig:Double): Double = {
    KS.kstwo(state.distribution, state.totalCounts, testCase.distribution, testCase.totalCounts)
  }

  def test(state: Probablistic, testCase: Probablistic, sig:Double): Boolean = nullHypothesis(state, testCase, sig) >= sig
}
