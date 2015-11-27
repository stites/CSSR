package com.typeclassified.cssr

import com.typeclassified.cssr.test.Test
import com.typeclassified.cssr.parse.{ParseNode, AlphabetHolder, ParseAlphabet, ParseTree}

import scala.collection.mutable.ListBuffer

package object CSSR {
  var parseTree: ParseTree = _
  var allStates: ListBuffer[CSSRState] = _
  var lMax: Int = 5
  var sig: Double = 0.7
  var emptyState: CSSRState = CSSRState()

  def main(args: Array[String]) = {
    parseTreeLoading()
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

  def initialization(): Unit = {
    AlphabetHolder.alphabet = ParseAlphabet(List('a', 'b'))
    allStates = ListBuffer(emptyState)
    // technically, this all that is needed in the "initialization" phase:
    // allStates = ListBuffer(emptyState)
  }

  def sufficiency(parseTree: ParseTree, S: ListBuffer[CSSRState], lMax: Int) = {
    for (l <- 1 to lMax) {
      for (xt <- parseTree.getDepth(l)) {
        val s = xt.currentState
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

  def recursion (parseTree: ParseTree, S: ListBuffer[CSSRState], lMax: Int) = {
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
                val temp:Option[ParseNode] = y.findChildWithAdditionalHistory(a)
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

  def equivalenceClass(hist:ParseNode) = {
    // TODO: equivalenceClass
  }
}

