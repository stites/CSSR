package com.typeclassified.cssr

import com.typeclassified.cssr.parse.{ParseTree, AlphabetHolder, ParseAlphabet}
import breeze.linalg._

import scala.collection.mutable.ListBuffer

object CSSR {
  var parseTree:ParseTree = _
  var Lmax:Int = _
  var sig:Double = _
  var emptyState:State = _
  var allStates:ListBuffer[State] = _

  def initialization (): Unit = {
    val A = List('a', 'b')
    Lmax = 5
    sig = 0.7
    emptyState = State(0.toChar)
    parseTree = ParseTree()

    AlphabetHolder.alphabet = ParseAlphabet(A)

    def parseTreeLoading() = {
      // initialize psuedo-observations:
      val datasize = 1000
      var obs:ListBuffer[Char] = new ListBuffer[Char]()
      1 to datasize foreach { i => obs += A(i%2) }

      ParseTree.loadData(parseTree, obs.toList, Lmax)
    }

    // technically, this all that is needed in the "initialization" phase:
    val allStates:List[State] = List(emptyState)
  }

  def main(args: Array[String]) = {
    /*
      def sufficiency():
        for L in range(1,Lmax):
          # get a list of parse_nodes with len(history) == L
          for Xt in parse_nodes:
            s = Xt.current_state
            for a in A:
              # NOTE: latent MMs
              # node in the parse tree with predictive dist
              aXt = Xt.children.find(lambda child: child.history[0] == a)
              s.normalize_across_histories()
              p = s.normalized_distribution
              test(S, p, aXt, s, sig)
     */
    println(parseTree)
  }
}

