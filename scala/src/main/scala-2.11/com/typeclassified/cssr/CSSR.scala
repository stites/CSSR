package com.typeclassified.cssr

import com.typeclassified.cssr.parse.{ParseNode, ParseTree, AlphabetHolder, ParseAlphabet}
import breeze.linalg._

import scala.collection.mutable.ListBuffer

package object CSSR {
  var parseTree:ParseTree = _
  var Lmax:Int = _
  var sig:Double = _
  var emptyState:CSSRState = _
  var allStates:ListBuffer[CSSRState] = _

  def initialization (): Unit = {
    AlphabetHolder.alphabet = ParseAlphabet(List('a', 'b'))
    Lmax = 5
    sig = 0.7
    emptyState = CSSRState()
    parseTree = ParseTree()

    def parseTreeLoading() = {
      // initialize psuedo-observations:
      val datasize = 1000
      var obs:ListBuffer[Char] = new ListBuffer[Char]()
      1 to datasize foreach { i => obs += AlphabetHolder.alphabet.alphabetRaw(i%2) }

      ParseTree.loadData(parseTree, obs.toList, Lmax)
    }

    // technically, this all that is needed in the "initialization" phase:
    allStates = ListBuffer(emptyState)
  }

  def sufficiency(parseTree: ParseTree, S:ListBuffer[CSSRState], Lmax:Int) = {
    for (l <- 1 to Lmax) {
      for (xt <- parseTree.getDepth(l)){
        var s = xt.currentState
        for ((a, v) <- AlphabetHolder.alphabet.map){
          // node in the parse tree with predictive dist
          var aXt = xt.children.find(child => child.history(0) == a)
          s.normalizeAcrossHistories()
          var p = s.normalDistribution
          Test.test(S, p, aXt, s, sig)
        }
      }
    }
  }

  def main(args: Array[String]) = {
    initialization()
    sufficiency(parseTree, allStates, Lmax)
    println(parseTree)
  }
}

