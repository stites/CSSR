package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.state.EquivalenceClass
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object Tree {
  protected val logger = Logger(LoggerFactory.getLogger(Tree.getClass))
  def apply(alphabet: Alphabet, equivalenceClass: EquivalenceClass) = new Tree(alphabet, equivalenceClass)
  def apply(alphabet: Alphabet) = new Tree(alphabet)

  def loadData(tree:Tree, xs: Array[Char], n: Int): Tree = {
    //  Yield successive n-sized windows from the x's. Does not work with a length of 0.
    logger.debug(s"loading data of size ${xs.length}")
    logger.debug("==> running over windows of (size, count): "+(1 to n).map(i => (i, xs.length/i)))

    for (size <- 1 to n+1) {
      logger.debug(s"loading data windows of size $size.")
      for (zippedSequence:Seq[(Char, Int)] <- xs.view.zipWithIndex.iterator.filterNot(p => "\r\n".contains(p._1)).sliding(size).withPartial(false)) {
        // TODO: check the following statement to see if we need to rollback below:
        // if n lies in the interesting region between n-1 and n then we also pass the index to the leaf
        // (THIS MAY ONLY BE NEEDED TO AVOID CLUTTER FOR LEAVES <n-1)
        loadHistory(tree, zippedSequence.map(_._1), Some(zippedSequence.last._2))
      }
    }
    tree.getDepth(n).foreach{ _.children = ListBuffer() }
    tree.maxLength = n
    tree.dataSize = xs.length
    tree.adjustedDataSize =  xs.length - n - 1 // TODO: multi-line
    return tree
  }

  def loadHistory(tree: Tree, observed: Seq[Char], idx:Option[Int] = None): Unit = {

    def go(history: List[Char], active:Leaf, tree: Tree, fullHistory:String, depth:Int=0): Option[Leaf] = {
      if (history.isEmpty) return Option.empty

      val maybeNext:Option[Leaf] = active.findChildWithAdditionalHistory(history.last)
      val histIdx:Int = depth+1

      if (maybeNext.nonEmpty) {
        if (history.init.isEmpty) active.updateDistribution(history.last, idx)

        return go(history.init, maybeNext.get, tree, fullHistory, histIdx)
      } else {
        val next = active.addChild(fullHistory(fullHistory.length - histIdx), idx)
        return go(history.init, next, tree, fullHistory, histIdx)
      }
    }

    go(observed.toList, tree.root, tree, observed.mkString)
  }
}

class Tree(val alphabet: Alphabet, rootEC: EquivalenceClass=EquivalenceClass()) {
  var root:Leaf = new Leaf("", this, rootEC)

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

  def collectLeaves(layer:ListBuffer[Leaf] = ListBuffer(root), collected:ListBuffer[Leaf]=ListBuffer()):Array[Leaf] = {
    if (layer.isEmpty) {
      return collected.toArray
    } else {
      val (foundLeaves, nextLayer) = layer.partition(_.children.isEmpty)
      collectLeaves(nextLayer.flatMap(_.children), collected ++ layer)
    }
  }
}


