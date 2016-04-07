package com.typeclassified.hmm.cssr.state

import breeze.linalg._
import com.typeclassified.hmm.cssr.shared.EmpiricalDistribution
import com.typeclassified.hmm.cssr.trees.{LLeaf, ParseLeaf}

class State extends EmpiricalDistribution[ParseLeaf] {

  def this(lLeaf:LLeaf) = {
    this()
    addTerminal(lLeaf)
  }

  var terminals:Set[LLeaf] = Set()

  def addTerminal(l: LLeaf): Unit = {
    terminals = terminals + l
    addHistories(l.histories)
  }

  def addHistory(h: ParseLeaf): Unit = addHistories(List(h))

  def addHistories(hs: List[ParseLeaf]): Unit = {
    histories = histories ++ hs
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

