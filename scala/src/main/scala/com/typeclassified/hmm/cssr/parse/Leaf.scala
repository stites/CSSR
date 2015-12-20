package com.typeclassified.hmm.cssr.parse

import breeze.linalg.{VectorBuilder, SparseVector}
import com.typeclassified.hmm.cssr.shared.Probablistic
import com.typeclassified.hmm.cssr.state.EquivalenceClass

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Leaf(observedSequence:String,
           parseTree: Tree,
           initialEquivClass: EquivalenceClass
          ) extends Probablistic {
  val observed:String = observedSequence.reverse

  val observation: Char = if ("".equals(observed)) 0.toChar else observed.last

  var currentEquivalenceClass: EquivalenceClass = initialEquivClass

  val locations:mutable.HashMap[Int, Int] = mutable.HashMap[Int, Int]()

  var children: ListBuffer[Leaf] = ListBuffer()

  def updateDistribution(xNext: Char, dataIdx:Option[Int] = None):Unit = {
    val idx: Int = parseTree.alphabet.map(xNext)

    frequency(idx) += 1
    totalCounts += 1
    distribution = frequency / totalCounts

    if (dataIdx.nonEmpty) {
      val indexCount = if (locations.keySet.contains(dataIdx.get)) locations(dataIdx.get) else 0
      locations += (dataIdx.get -> (indexCount + 1))
    }
  }

  def addChild (xNext:Char, dataIdx:Option[Int] = None): Leaf = {
    updateDistribution(xNext, dataIdx)
    val maybeNext = findChildWithAdditionalHistory(xNext)
    var next:Leaf = null
    if (maybeNext.isEmpty) {
      next = new Leaf(xNext+:observed, parseTree, currentEquivalenceClass)
      children += next
    } else {
      next = maybeNext.get
    }
    return next
  }

  def changeEquivalenceClass(s: EquivalenceClass): Unit = {
    this.currentEquivalenceClass = s
    // we ought to update transitions here (but for phase II it's not terribly important)
    this.children.foreach(_.changeEquivalenceClass(s))
  }

  def findChildWithAdditionalHistory(xNext: Char):Option[Leaf] = {
    // to find BA
    // node A, search for child with B
    children.find(_.observation == xNext)
  }

  def getStateOnTransitionTo(b:Char):Option[EquivalenceClass] = {
    val optionalLeaf = parseTree.navigateHistory((this.observed + b).toList)

    return if (optionalLeaf.nonEmpty) Option.apply(optionalLeaf.get.currentEquivalenceClass) else Option.empty
  }
}

