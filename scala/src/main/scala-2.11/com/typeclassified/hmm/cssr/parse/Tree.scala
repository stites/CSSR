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

    for (size <- 1 to n) {
      logger.debug(s"loading data windows of size $size.")
      for (observed <- xs.iterator.filterNot("\r\n".contains(_)).sliding(size).withPartial(false)) {
        loadHistory(tree, observed)
      }
    }
    tree.maxLength = n
    tree.dataSize = xs.length
    tree.adjustedDataSize =  xs.length - n - 1 // TODO: multi-line
    return tree
  }


  def loadHistory(tree: Tree, observed: Seq[Char]): Unit = {

    def go(history: List[Char], active:Leaf, tree: Tree, fullHistory:String, depth:Int=0): Option[Leaf] = {
      if (history.isEmpty) return Option.empty
      val maybeNext:Option[Leaf] = active.findChildWithAdditionalHistory(history.last)
      var next:Leaf = null

      // check nonEmpty here for the update, since delegation checks history first
      if (maybeNext.nonEmpty) maybeNext.get.updateDistribution(history.head)

      if (history.isEmpty) {
        return maybeNext
      } else {
        val histIdx:Int = depth+1
        val histLen:Int = fullHistory.length

        if (maybeNext.isEmpty) {
          val obs = fullHistory.slice(histLen-histIdx, histLen)
          val next = Leaf(obs, tree, active.currentEquivalenceClass)
          active.children += next
          return go(history.init, next, tree, fullHistory, histIdx)
        } else {
          return go(history.init, maybeNext.get, tree, fullHistory, histIdx)
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
    if (history.isEmpty) Option.empty else {
      val maybeNext:Option[Leaf] = active.findChildWithAdditionalHistory(history.last)

      if (history.init.isEmpty || maybeNext.isEmpty) {
        return maybeNext
      } else {
        return navigateHistory(history.init, maybeNext.get)
      }
    }
  }

  def getDepth(depth: Int, nodes:ListBuffer[Leaf] = ListBuffer(root)): Array[Leaf] = {
    if (depth <= 0) {
      nodes.toArray
    } else {
      getDepth(depth-1, nodes.flatMap(_.children))
      // [nodes] => [children] ++ [children]
    }
  }
}


