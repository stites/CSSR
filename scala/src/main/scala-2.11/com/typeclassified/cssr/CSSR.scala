package com.typeclassified.cssr

import com.typeclassified.cssr.test.Test
import parse.{AlphabetHolder, ParseAlphabet, ParseTree}

import scala.collection.mutable.ListBuffer

package object CSSR {
  var parseTree: ParseTree = _
  var lMax: Int = _
  var sig: Double = _
  var emptyState: CSSRState = _
  var allStates: ListBuffer[CSSRState] = _

  def main(args: Array[String]) = {
    initialization()
    sufficiency(parseTree, allStates, lMax)
    println(parseTree)
  }

  def initialization(): Unit = {
    AlphabetHolder.alphabet = ParseAlphabet(List('a', 'b'))
    lMax = 5
    sig = 0.7
    emptyState = CSSRState()
    parseTree = ParseTree()

    def parseTreeLoading() = {
      // initialize psuedo-observations:
      val dataSize = 1000
      var obs: ListBuffer[Char] = new ListBuffer[Char]()
      1 to dataSize foreach { i => obs += AlphabetHolder.alphabet.alphabetRaw(i % 2) }

      ParseTree.loadData(parseTree, obs.toList, lMax)
    }

    // technically, this all that is needed in the "initialization" phase:
    allStates = ListBuffer(emptyState)
  }

  def sufficiency(parseTree: ParseTree, S: ListBuffer[CSSRState], lMax: Int) = {
    for (l <- 1 to lMax) {
      for (xt <- parseTree.getDepth(l)) {
        val s = xt.currentState
        for ((a, v) <- AlphabetHolder.alphabet.map) {
          // node in the parse tree with predictive dist
          val aXt = xt.children.find(child => child.history(0) == a)
          s.normalizeAcrossHistories()
          val p = s.normalDistribution
          Test.test(S, p, aXt.get, s, sig)
        }
      }
    }
  }
}

