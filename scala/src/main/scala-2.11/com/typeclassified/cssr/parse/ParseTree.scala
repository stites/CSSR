package com.typeclassified.cssr.parse

import com.typeclassified.cssr.{CausalState, EquivalenceClass}

import scala.collection.mutable.ListBuffer

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
  var root:CausalState = CausalState(0.toChar, this, EquivalenceClass())

  /**
    * navigate from root to x_hist leaf and update the distribution with x0
    * @param x0
    * @param x_hist
    */
  def updatePredictiveDistribution(x0: Char, x_hist: List[Char]):Unit = {
    val maybeState = navigateHistory(x_hist)
    if (maybeState.nonEmpty) {
      maybeState.get.updateDistribution(x0)
    }
  }

  def navigateHistory(history: List[Char]): Option[CausalState] = {
    def subroutine(active:CausalState, history:List[Char]): CausalState = {
      val maybeNext:Option[CausalState] = active.findChildWithAdditionalHistory(history.head)
      var next:CausalState = null

      if (maybeNext.nonEmpty) {
        next = maybeNext.get
      } else {
        next = CausalState(history.head, this, active.currentEquivalenceClass)
        active.children += next
      }

      return if (history.tail.isEmpty) next else subroutine(next, history.tail)
    }
    return if (history.isEmpty) Option.empty else Option.apply(subroutine(root, history))
  }

  def getDepth(depth: Int): Array[CausalState] = {
    def subroutine(nodes: ListBuffer[CausalState], depth: Int): Array[CausalState] = {
      if (depth <= 0) {
        nodes.toArray
      } else {
        subroutine(nodes.flatMap(_.children), depth - 1)
      }
    }
    return subroutine(root.children, depth)
  }
}


