package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.{Leaf, Leaf$, EquivalenceClass}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object ParseTree {
  protected val logger = Logger(LoggerFactory.getLogger(ParseTree.getClass))
  def apply() = new ParseTree()

  def loadData(xs: Array[Char], n: Int): ParseTree = {
    val tree:ParseTree = new ParseTree()
    //  Yield successive n-sized windows from the x's. Does not work with a length of 0.
    logger.debug(s"loading data of size ${xs.length}")
    logger.debug("==> running over windows of (size, count): "+(1 to n).map(i => (i, xs.length/i)))

    for (size <- 1 until n) {
      logger.debug(s"loading data windows of size $size.")
      for (observed <- xs.iterator.sliding(size).withPartial(false)) {
        // updates the predictive distributions of the tree
        // ABC
        // C, AB
        tree.updatePredictiveDistribution(observed.last, observed.init.toList) //TODO: flip these args
      }
    }
    tree.maxLength = n
    tree.dataSize = xs.length
    tree.adjustedDataSize =  xs.length - n - 1 // TODO: multi-line
    return tree
  }
}

class ParseTree {
  var root:Leaf = Leaf("", this, EquivalenceClass())
  var maxLength:Int = _
  var dataSize:Double = _
  var adjustedDataSize:Double = _

  /**
    * navigate from root to x_hist leaf and update the distribution with x0
    * => ParseTree goes deeper into the past
    *
    *
    * @param x0
    * @param x_hist
    */
  def updatePredictiveDistribution(x0: Char, x_hist: List[Char]):Unit = {
    val maybeLeaf = navigateHistory(x_hist)
    if (maybeLeaf.nonEmpty) {
      maybeLeaf.get.updateDistribution(x0)
    }
  }

  // ABC, ABB => [{C->B->A},{B->C->A}]
  def navigateHistory(history: List[Char]): Option[Leaf] = {
    def subroutine(active:Leaf, history:List[Char]): Leaf = {
      val maybeNext:Option[Leaf] = active.findChildWithAdditionalHistory(history.last)
      var next:Leaf = null

      if (maybeNext.nonEmpty) {
        next = maybeNext.get
      } else {
        next = Leaf(history.mkString, this, active.currentEquivalenceClass)
        active.children += next
      }

      return if (history.init.isEmpty) next else subroutine(next, history.init)
    }

    return if (history.isEmpty) Option.empty else Option.apply(subroutine(root, history))
  }

  def getDepth(depth: Int): Array[Leaf] = {
    def subroutine(nodes: ListBuffer[Leaf], depth: Int): Array[Leaf] = {
      if (depth <= 0) {
        nodes.toArray
      } else {
        subroutine(nodes.flatMap(_.children), depth - 1)
        // [nodes] => [children] ++ [children]
      }
    }
    return subroutine(root.children, depth)
  }
}


