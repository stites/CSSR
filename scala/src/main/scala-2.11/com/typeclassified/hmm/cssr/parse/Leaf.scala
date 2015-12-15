package com.typeclassified.hmm.cssr.parse

import breeze.linalg.normalize
import com.typeclassified.hmm.cssr.shared.Probablistic
import com.typeclassified.hmm.cssr.state.EquivalenceClass

import scala.collection.mutable.ListBuffer

object Leaf {
  def apply(o:String, tree: Tree, initState: EquivalenceClass) = new Leaf(o, tree, initState)
}

class Leaf(observedSequence:String,
           parseTree: Tree,
           initialEquivClass: EquivalenceClass
          ) extends Probablistic {
  val observed:String = observedSequence.reverse

  val observation: Char = if ("".equals(observed)) 0.toChar else observed.last

  var currentEquivalenceClass: EquivalenceClass = initialEquivClass

  var children: ListBuffer[Leaf] = ListBuffer()

  def updateDistribution(xNext: Char):Unit = {
    val idx: Int = parseTree.alphabet.map(xNext)
    frequency(idx) += 1
    totalCounts += 1
    distribution = frequency / totalCounts
  }

  def addChild (xNext:Char): Leaf = {
    updateDistribution(xNext)
    val maybeNext = findChildWithAdditionalHistory(xNext)
    var next:Leaf = null
    if (maybeNext.isEmpty) {
      next = Leaf(xNext+:observed, parseTree, currentEquivalenceClass)
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

