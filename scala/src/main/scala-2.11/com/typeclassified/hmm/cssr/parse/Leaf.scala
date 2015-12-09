package com.typeclassified.hmm.cssr.parse

import breeze.linalg.normalize
import com.typeclassified.hmm.cssr.{EquivalenceClass, Probablistic}

import scala.collection.mutable.ListBuffer

object Leaf {
  def apply(o:String, tree: Tree, initState: EquivalenceClass) = new Leaf(o, tree, initState)
}

class Leaf(val observed:String,
           parseTree: Tree,
           initialEquivClass: EquivalenceClass
          ) extends Probablistic {
  // (history = 00) => (next_x = 1) ==> 001
  val observation: Char = if ("".equals(observed)) 0.toChar else observed.head // C
  // always initialize a class with an updated distribution on its observation
  if (!"".equals(observed)) updateDistribution(observation)

  var currentEquivalenceClass: EquivalenceClass = initialEquivClass

  var children: ListBuffer[Leaf] = ListBuffer()

  def updateDistribution(xNext: Char):Unit = {
    val idx: Int = AlphabetHolder.alphabet.map(xNext)
    frequency(idx) += 1
    totalCounts += 1
    distribution = normalize(frequency)
  }

  def changeEquivalenceClass(s: EquivalenceClass): Unit = {
    // s.append(this) # see null hypothesis and uncomment one
    currentEquivalenceClass = s
    // we ought to update transitions here (but for phase II it's not terribly important)
    children foreach (_.changeEquivalenceClass(s))
  }

  def findChildWithAdditionalHistory(xNext: Char):Option[Leaf] = {
    // to find BA
    // node A, search for child with B
    children.find(_.observation == xNext)
  }

  def getStateOnTransition(b:Char):Option[EquivalenceClass] = {
    val optionalLeaf = parseTree.navigateHistory((this.observed + b).toList)

    return if (optionalLeaf.nonEmpty) Option.apply(optionalLeaf.get.currentEquivalenceClass) else Option.empty
  }

  def longestHistories(traverse: Array[Leaf] = Array(this), collected:Array[String]= Array()): Array[String] = {
    if (traverse.isEmpty) {
      return collected
    } else {
      val (lastLeaf, next) = traverse.partition(_.children.isEmpty)
      return longestHistories(next.flatMap(_.children), (collected ++ lastLeaf.map(_.observed)).distinct)
    }
  }
}

