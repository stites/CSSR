package com.typeclassified.cssr

import com.typeclassified.cssr.parse.{AlphabetHolder, ParseTree}

import scala.collection.mutable.ListBuffer

object CausalState {
  def apply(c: Char, tree: ParseTree, initState: EquivalenceClass) = new CausalState(c, tree, initState)
}

class CausalState(observation: Char, parseTree: ParseTree, initialEquivClass: EquivalenceClass) extends Probablistic {
  /* history = 00
   * next_x  = 1
   *       ==> 001
   */
  val value: Char = observation
  var currentEquivalenceClass: EquivalenceClass = initialEquivClass
  var children: ListBuffer[CausalState] = ListBuffer()

  def updateDistribution(xNext: Char):Unit = {
    val idx: Int = AlphabetHolder.alphabet.map(xNext)
    frequency(idx) += 1
    totalCounts += 1
    normalDistribution = frequency :/ totalCounts.toDouble
  }

  def changeEquivalenceClass(s: EquivalenceClass): Unit = {
    // s.append(this) # see null hypothesis and uncomment one
    currentEquivalenceClass = s
    // we ought to update transitions here (but for phase II it's not terribly important)
    children foreach (child => child.changeEquivalenceClass(s))
  }

  def findChildWithAdditionalHistory(xNext: Char):Option[CausalState] = {
    children.find(child => child.value == xNext)
  }
}

