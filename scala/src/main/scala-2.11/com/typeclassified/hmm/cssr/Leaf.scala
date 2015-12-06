package com.typeclassified.hmm.cssr

import breeze.linalg.normalize
import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, ParseTree}

import scala.collection.mutable.ListBuffer

object Leaf {
  def apply(o:String, tree: ParseTree, initState: EquivalenceClass) = new Leaf(o, tree, initState)
}

class Leaf(val observed:String,
           parseTree: ParseTree,
           initialEquivClass: EquivalenceClass
                 ) extends Probablistic {
  /* history = 00
   * next_x  = 1
   *       ==> 001
   */
  val observation: Char = if (observed == "") 0.toChar else observed.last // C
  val history:String = if (observed == "") "" else observed.init // ABC -> AB
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
    children foreach (child => child.changeEquivalenceClass(s))
  }

  def findChildWithAdditionalHistory(xNext: Char):Option[Leaf] = {
    // to find BA
    // node A, search for child with B
    children.find(child => child.observation == xNext)
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
      return longestHistories(next.flatMap(_.children), lastLeaf.map(_.observed))
    }
  }
}

