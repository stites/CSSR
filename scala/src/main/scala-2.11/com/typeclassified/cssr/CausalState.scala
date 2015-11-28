package com.typeclassified.cssr

import com.typeclassified.cssr.parse.{AlphabetHolder, ParseTree}

object CausalState {
  def apply(str: String, tree: ParseTree, initState: EquivalenceClass) = new CausalState(str, tree, initState)
}

class CausalState(string: String, parseTree: ParseTree, initializingState: EquivalenceClass) extends Probablistic {
  /* history = 00
   * next_x  = 1
   *       ==> 001
   */
  val history: String = string
  var currentEquivalenceClass: EquivalenceClass = initializingState
  var children: List[CausalState] = List()

  def updateDistribution(xNext: Char) = {
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
    children.find(child => child.history(0) == xNext)
  }
}

