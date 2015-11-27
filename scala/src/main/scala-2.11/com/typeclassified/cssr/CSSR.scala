package com.typeclassified.cssr

import com.typeclassified.cssr.test.Test
import parse.{AlphabetHolder, ParseAlphabet, ParseTree}

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
          val aXt = xt.children.find(child => child.history(0) == a)
          s.normalizeAcrossHistories()
          val p = s.normalDistribution(alphaIdx)
          Test.test(S, p, aXt.get, s, sig)
        }
      }
    }
  }
}

