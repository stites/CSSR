package com.typeclassified.cssr

import breeze.linalg.normalize
import com.typeclassified.cssr.parse.{AlphabetHolder, ParseTree}

import scala.collection.mutable.ListBuffer

object CausalState {
  def apply(o:String, tree: ParseTree, initState: EquivalenceClass) = new CausalState(o, tree, initState)
}

class CausalState(val observed:String,
                  parseTree: ParseTree,
                  initialEquivClass: EquivalenceClass
                 ) extends Probablistic {
  /* history = 00
   * next_x  = 1
   *       ==> 001
   */
  val observation: Char = if (observed == "") 0.toChar else observed.head
  val history:String = if (observed == "") "" else observed.tail
  var currentEquivalenceClass: EquivalenceClass = initialEquivClass
  var children: ListBuffer[CausalState] = ListBuffer()

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

  def findChildWithAdditionalHistory(xNext: Char):Option[CausalState] = {
    children.find(child => child.observation == xNext)
  }
}

