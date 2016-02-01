package com.typeclassified.hmm.cssr.parse

import breeze.linalg.DenseVector
import com.typeclassified.hmm.cssr.state.EquivalenceClass
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer


/*
We encounter the history say "110"
We go to the parse tree at the root
We take the 0 child of the root
We then take the 1 child of 0 (= 10)
We then take the 1 child of 10 (=110)
*/

object Tree {
  protected val logger = Logger(LoggerFactory.getLogger(Tree.getClass))
  def apply(alphabet: Alphabet, equivalenceClass: EquivalenceClass) = new Tree(alphabet, equivalenceClass)
  def apply(alphabet: Alphabet) = new Tree(alphabet)

  def loadData(tree:Tree, xs: Array[Char], n: Int): Tree = {
    val banned = "\r\n".toSet

    for (obs:Seq[Char] <- xs.view
      .iterator
      .filterNot(banned.contains)
      .sliding(n+1)
      .withPartial(true)) {
      insertTo(tree, obs)
    }

    val lastIdx = xs.length - (n + 1)
    for (i <- 0 until n) {
      val qs = xs.slice(lastIdx + i, xs.length).filterNot(banned.contains)
      insertTo(tree, qs)
    }

    // gross, but alas, on a deadline:
    val filteredCharactersCount = xs.count(banned.contains)
    tree.maxLength = n
    tree.dataSize = xs.length - filteredCharactersCount
    tree.adjustedDataSize =  xs.length - (n - 1) - filteredCharactersCount

    // calculate initial histories
    for (depth <- 0 to n) {
      tree.getDepth(depth).foreach(_.calcNextStepProbabilities())
    }

    // FIXME: sketchy?
    tree.root.frequency = new DenseVector[Double](tree.root.frequency.toArray.reverse)
    tree.root.distribution = tree.root.frequency / tree.root.totalCounts

    tree.getDepth(n).foreach{ _.children = ListBuffer() }
    tree
  }

  // FIXME: sketchy?
  def insertTo(tree: Tree, observed: Seq[Char], idx:Option[Int] = None): Unit = {
    def go(history: List[Char] /*102*/ , active: Leaf, tree: Tree, fullHistory: String, depth: Int = 0): Unit = {
      if (history.nonEmpty) {
        // FIXME: sketchy?
        // Note: This is the reverse of expected!!!
        val maybeNext: Option[Leaf] = active.findChildWithAdditionalHistory(history.head)
        val next =  if (maybeNext.isEmpty) active.addChild(history.head) else maybeNext.get
        if (maybeNext.nonEmpty) next.obsCount += 1
        go(history.tail, next, tree, fullHistory)
      }
    }

    go(observed.toList, tree.root, tree, observed.mkString)
  }

  @Deprecated
  def loadHistoryOnce(tree: Tree, observed: Seq[Char], idx:Option[Int] = None): Unit = {
    // 102       oldest -> newest
    // NULL -> 2 -> 0 -> 1
    def go(history: List[Char] /*102*/ , active: Leaf, tree: Tree, fullHistory: String, depth: Int = 0): Unit = {
      if (history.nonEmpty) {
        val maybeNext: Option[Leaf] = active.findChildWithAdditionalHistory(history.last) /*2*/
        val histIdx: Int = depth + 1
        val next = if (maybeNext.nonEmpty) maybeNext.get else active.addChild(fullHistory(fullHistory.length - histIdx) /* 102 => fullHistory(3-1) => FH(2)*/ , idx /*102*/)
        active.incrementDistribution(next.observation, idx)
        go(history.init /*10*/ , next, tree, fullHistory /*102*/ , histIdx)
      }
    }

    go(observed.toList, tree.root, tree, observed.mkString)
  }

  @Deprecated
  def loadHistory(tree: Tree, observed: Seq[Char], idx:Option[Int] = None): Unit = {
    // 102       oldest -> newest
    // NULL -> 2 -> 0 -> 1
    def go(history: List[Char] /*102*/ , active: Leaf, tree: Tree, fullHistory: String, depth: Int = 0): Option[Leaf] = {
      if (history.isEmpty) return None
      val maybeNext: Option[Leaf] = active.findChildWithAdditionalHistory(history.last) /*2*/
      val histIdx: Int = depth + 1
      var next:Leaf = null

      if (maybeNext.nonEmpty) {
        if (history.init.isEmpty) active.incrementDistribution(history.last, idx)
        next = maybeNext.get
      } else {
        /*102     => fullHistory(3-1) => FH(2)*/
        next = active.addChild(fullHistory(fullHistory.length - histIdx) /*2*/ , idx /*102*/)
      }
      go(history.init /*10*/ , next, tree, fullHistory /*102*/ , histIdx)
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
  @Deprecated
  def updatePredictiveDistribution(observed: List[Char]):Unit = {
    val (hist, x0) = (observed.init, observed.last)
    val maybeLeaf = navigateHistory(hist)
    if (maybeLeaf.nonEmpty) {
      maybeLeaf.get.incrementDistribution(x0)
    }
  }

  // ABC, ABB => [{C->B->A},{B->C->A}]
  def navigateHistory(history: List[Char], active:Leaf = root): Option[Leaf] = {
    if (history.isEmpty) None else {
      val maybeNext:Option[Leaf] = active.findChildWithAdditionalHistory(history.last)
      if (history.init.isEmpty || maybeNext.isEmpty) {
        maybeNext
      } else {
        navigateHistory(history.init, maybeNext.get)
      }
    }
  }

  def findStrings(depth: Int, nodes:ListBuffer[Leaf] = ListBuffer(root)): Array[Leaf] = {
    if (depth <= 0) {
      nodes.toArray
    } else {
      // [nodes] => [children] ++ [children]
      getDepth(depth-1, nodes.flatMap(_.children))
    }
  }

  def getDepth(depth: Int, nodes:ListBuffer[Leaf] = ListBuffer(root)): Array[Leaf] = {
    if (depth <= 0) {
      nodes.toArray
    } else {
      // [nodes] => [children] ++ [children]
      getDepth(depth-1, nodes.flatMap(_.children))
    }
  }

  def collectLeaves(layer:ListBuffer[Leaf] = ListBuffer(root), collected:ListBuffer[Leaf]=ListBuffer()):Array[Leaf] = {
    if (layer.isEmpty) {
      collected.toArray
    } else {
      val (foundLeaves, nextLayer) = layer.partition(_.children.isEmpty)
      collectLeaves(nextLayer.flatMap(_.children), collected ++ layer)
    }
  }
}


