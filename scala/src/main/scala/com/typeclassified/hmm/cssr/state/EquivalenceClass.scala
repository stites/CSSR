package com.typeclassified.hmm.cssr.state

import breeze.linalg._
import com.typeclassified.hmm.cssr.shared.Probablistic
import com.typeclassified.hmm.cssr.trees.ParseLeaf

import scala.collection.immutable.TreeSet
import scala.collection.mutable

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

class EquivalenceClass extends State[ParseLeaf] {
  def addHistory(h: ParseLeaf): Unit = {
    histories = histories ++ List(h)
    recalculateHists(histories, DenseVector.zeros[Double](size))
  }

  def rmHistory(x: ParseLeaf): Unit = {
    histories = histories.filter(y => y.observed != x.observed)
    recalculateHists(histories, DenseVector.zeros[Double](size))
  }

  def shortString: String = {
    s"State ${hashCode()} {size:${histories.size}}"
  }

  def fullString: String = {
    s"${getClass.getSimpleName}@${hashCode()} {size:${histories.size}, ${histories.map(_.observed).mkString("[",", ","]")}}"
  }

  override def toString: String = fullString
}

