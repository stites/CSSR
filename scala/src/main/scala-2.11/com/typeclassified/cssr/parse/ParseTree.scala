package com.typeclassified.cssr.parse

import com.typeclassified.cssr.{CausalState, CSSR, EquivalenceClass, Probablistic}

import scala.collection.mutable.ArrayBuffer

object ParseTree {
  def apply() = new ParseTree()

  def loadData(tree: ParseTree, xs: List[Char], n: Int) = {
    //  Yield successive n-sized windows from the x's. Does not work with a length of 0.
    for (size <- 1 until n) {
      for (observed <- xs.iterator.sliding(size).withPartial(false)) {
        // updates the predictive distributions of the tree
        tree.updatePredictiveDistribution(observed.head, observed.tail.toList)
      }
    }
  }
}

class ParseTree {
  var root: ArrayBuffer[CausalState] = ArrayBuffer()

  def updatePredictiveDistribution(x0: Char, x_hist: List[Char]) = {
    // navigate from root to x_hist-leaf and update the distribution with x0
    navigateHistory(x_hist).updateDistribution(x0)
  }

  def navigateHistory(history: List[Char]): CausalState = {
    // TODO
    CausalState("dummy", this, CSSR.emptyState)
  }

  def getDepth(depth: Int): Array[CausalState] = {
    def subroutine(nodes: ArrayBuffer[CausalState], depth: Int): Array[CausalState] = {
      if (depth <= 0) nodes.toArray
      else subroutine(nodes.flatMap(_.children), depth - 1)
    }
    subroutine(root, depth)
  }
}


