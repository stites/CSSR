package com.typeclassified.hmm.cssr.state

import com.typeclassified.hmm.cssr.shared.EmpiricalDistribution
import com.typeclassified.hmm.cssr.trees.LLeaf

class State extends EmpiricalDistribution {
  def this(lLeaf:LLeaf) = {
    this()
    addTerminal(lLeaf)
  }

  var terminals:Set[LLeaf] = Set()

  def addTerminal(l: LLeaf): Unit = {
    terminals = terminals + l
    addHistories(l.histories)
  }

  def shortString: String = {
    s"State ${hashCode()} {size:${histories.size}}"
  }

  def fullString: String = {
    s"${getClass.getSimpleName}@${hashCode()} {size:${histories.size}, ${histories.map(_.observed).mkString("[",", ","]")}}"
  }

  override def toString: String = fullString
}

