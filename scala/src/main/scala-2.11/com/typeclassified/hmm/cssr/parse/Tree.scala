package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.EquivalenceClass
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object Tree {
  protected val logger = Logger(LoggerFactory.getLogger(Tree.getClass))
  def apply() = new Tree()

  def loadData(xs: Array[Char], n: Int): Tree = {
    val tree:Tree = new Tree()
    //  Yield successive n-sized windows from the x's. Does not work with a length of 0.
    logger.debug(s"loading data of size ${xs.length}")
    logger.debug("==> running over windows of (size, count): "+(1 to n).map(i => (i, xs.length/i)))

    for (size <- 1 until n) {
      logger.debug(s"loading data windows of size $size.")
      for (observed <- xs.iterator.sliding(size).withPartial(false)) {
        // updates the predictive distributions of the tree
        // ABC
        // C, AB
        loadHistory(tree, observed)
      }
    }
    tree.maxLength = n
    tree.dataSize = xs.length
    tree.adjustedDataSize =  xs.length - n - 1 // TODO: multi-line
    return tree
  }


  def loadHistory(tree: Tree, observed: Seq[Char]): Unit = {

    def go(history: List[Char], active:Leaf, tree: Tree, fullHistory:String): Option[Leaf] = {
      if (history.isEmpty) return Option.empty

      val maybeNext:Option[Leaf] = active.findChildWithAdditionalHistory(history.last)
      var next:Leaf = null
      if (history.isEmpty) {
        return maybeNext
      } else {
        if (maybeNext.isEmpty) {
          val histYoungest = fullHistory.length
          val histOldest = histYoungest - (fullHistory.length - history.length)
          val obs = history.slice(histOldest, histYoungest).mkString
          val next = Leaf(obs, tree, active.currentEquivalenceClass)
          active.children += next
          return go(history.init, next, tree, fullHistory)
        } else {
          return go(history.init, maybeNext.get, tree, fullHistory)
        }
      }
    }

    go(observed.toList, tree.root, tree, observed.mkString)
  }
}

class Tree {
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
  def updatePredictiveDistribution(observed: List[Char]):Unit = {
    val (hist, x0) = (observed.init, observed.last)
    val maybeLeaf = navigateHistory(hist)
    if (maybeLeaf.nonEmpty) {
      maybeLeaf.get.updateDistribution(x0)
    }
  }

  // ABC, ABB => [{C->B->A},{B->C->A}]
  def navigateHistory(history: List[Char], active:Leaf = root): Option[Leaf] = {
    val maybeNext:Option[Leaf] = active.findChildWithAdditionalHistory(history.last)

    if (maybeNext.isEmpty) {
      return Option.empty
    } else if (history.isEmpty) {
      return maybeNext
    } else {
      return navigateHistory(history.init, maybeNext.get)
    }
  }

  def getDepth(depth: Int, nodes:ListBuffer[Leaf] = root.children): Array[Leaf] = {
    if (depth <= 0) {
      nodes.toArray
    } else {
      getDepth(depth-1, nodes.flatMap(_.children))
      // [nodes] => [children] ++ [children]
    }
  }
}


