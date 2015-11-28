package com.typeclassified.cssr

import com.typeclassified.cssr.test.Test
import com.typeclassified.cssr.parse.{AlphabetHolder, ParseAlphabet, ParseTree}

import scala.collection.mutable.ListBuffer

package object CSSR {
  var parseTree: ParseTree = _
  var allStates: ListBuffer[EquivalenceClass] = _
  var lMax: Int = 5
  var sig: Double = 0.7
  var emptyState: EquivalenceClass = EquivalenceClass()

  def main(args: Array[String]) = {
    initialization()
    sufficiency(parseTree, allStates, lMax)
    recursion (parseTree, allStates, lMax)
    println(parseTree)
  }

  def parseTreeLoading() = {
    // initialize psuedo-observations:
    val dataSize = 1000
    var obs: ListBuffer[Char] = new ListBuffer[Char]()
    1 to dataSize foreach { i => obs += AlphabetHolder.alphabet.alphabetRaw(i % 2) }
    ParseTree.loadData(parseTree, obs.toList, lMax)
  }

  /**
    * In Initialization, specifically of the parse tree, we are
    * iterating through the different histories and generating
    * causal states which each contain a next-step probability
    * distribution.
    */
  def initialization(): Unit = {
    AlphabetHolder.alphabet = ParseAlphabet(List('a', 'b'))
    allStates = ListBuffer(emptyState)
    parseTreeLoading()
    // technically, this all that is needed in the "initialization" phase:
    // allStates = ListBuffer(emptyState)
  }

  /**
    * In Sufficiency, we will step through the causal states and
    * generate equivalence classes (ie causal state machine)
    * containing the aggregate of next-step probability distributions
    * (sufficiency statistics) from the causal states.
    *
    * Equivalence classes will grow as a result of testing.
    *
    * @param parseTree
    * @param S
    * @param lMax
    */
  def sufficiency(parseTree: ParseTree, S: ListBuffer[EquivalenceClass], lMax: Int) = {
    for (l <- 1 to lMax) {
      for (xt <- parseTree.getDepth(l)) {
        val s = xt.currentEquivalenceClass
        for ((a, alphaIdx) <- AlphabetHolder.alphabet.map) {
          // node in the parse tree with predictive dist
          val aXt = xt.findChildWithAdditionalHistory(a)
          s.normalizeAcrossHistories()
          val p = s.normalDistribution(alphaIdx)
          Test.test(S, p, aXt.get, s, sig)
        }
      }
    }
  }

  /**
    * In Recursion, we will verify that our equivalence classes are
    * correct. This is done by taking an equivalence class' causal
    * state (the first one is fine), adding new information to it
    * (from the alphabet), and comparing its probability distribution
    * (as a baseline) to all other causal states' estimates with this
    * new information.
    *
    * If there is a difference of the estimations from the baseline,
    * move the offending causal state to a new equivalence class.
    *
    * If there are no differences, we conclude.
    *
    * @param parseTree
    * @param S
    * @param lMax
    */
  def recursion (parseTree: ParseTree, S: ListBuffer[EquivalenceClass], lMax: Int) = {
    var recursive = false
    while (!recursive) {
      recursive = true
      for (s <- S) {
        for ((a, alphabetIdx) <- AlphabetHolder.alphabet.map) {
          val x0 = s.histories(0)
          val x0a = x0.findChildWithAdditionalHistory(a).get
          val TransitionStateA = equivalenceClass(x0a)
          for (x <- s.histories.tail) {
            val TransitionStateB = equivalenceClass(x)
            if (TransitionStateA.value != TransitionStateB.value) {
              val sNew = emptyState
              allStates += sNew
              val TransitionStateBNew = TransitionStateB
              for (y <- s.histories) {
                val temp:Option[CausalState] = y.findChildWithAdditionalHistory(a)
                if (temp.get == TransitionStateBNew) {
                  Test.move(y, s, sNew)
                }
              }
              recursive = false
            }
          }
        }
      }
    }
  }

  def equivalenceClass(hist:CausalState) = {
    // TODO: equivalenceClass
  }
}

