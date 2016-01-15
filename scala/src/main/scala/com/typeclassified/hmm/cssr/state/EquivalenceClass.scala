package com.typeclassified.hmm.cssr.state

import breeze.linalg._
import com.typeclassified.hmm.cssr.parse.Leaf
import com.typeclassified.hmm.cssr.shared.Probablistic

object EquivalenceClass {
  def apply() = new EquivalenceClass()

  /**
    * Note that a {@link EquivalenceClass#State} and an {@link EquivalenceClass}  are treated different only by
    * semantics. An "Equivalence Class" only exists when we have *no* states and can only conjecture to what a possible
    * state may be. We use the term "State" when we have more concrete evidence to support such claims.
    *
    * This means that you'll find "Equivalence Class" in most places in CSSR. In {@link Machine} we take the results of
    * CSSR's equivalence classes and measure them -- at this point we hypothesize that we have, indeed, found the
    * underlying states, and it becomes more natural to call them "States".
    */
  type State = EquivalenceClass
}

class EquivalenceClass extends Probablistic {
  var histories: Set[Leaf] = Set()

  def addHistory(h: Leaf): Unit = {
    histories += h
    normalizeAcrossHistories()
  }

  def rmHistory(x: Leaf): Unit = {
    histories = histories.filter(y => y.observed != x.observed)
    normalizeAcrossHistories()
  }

  def normalizeAcrossHistories(): Unit = {
    frequency = histories.foldRight(DenseVector.zeros[Double](size))((history, totalFreq) => totalFreq + history.frequency)

    totalCounts = frequency.foldRight(0d)(_+_).toInt

    distribution = if (totalCounts == 0) DenseVector.zeros(frequency.length) else frequency / totalCounts
  }
}

