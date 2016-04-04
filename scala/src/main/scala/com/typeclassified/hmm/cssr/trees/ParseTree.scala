package com.typeclassified.hmm.cssr.trees

import breeze.linalg.{DenseVector, sum}
import com.typeclassified.hmm.cssr.parse.Alphabet
import com.typeclassified.hmm.cssr.state.EquivalenceClass
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/*
We encounter the history say "110"
We go to the parse tree at the root
We take the 0 child of the root
We then take the 1 child of 0 (= 10)
We then take the 1 child of 10 (=110)
*/
object ParseTree extends LazyLogging {
  def apply(alphabet: Alphabet, equivalenceClass: EquivalenceClass) = new ParseTree(alphabet, equivalenceClass)
  def apply(alphabet: Alphabet) = new ParseTree(alphabet)

  def loadData(tree:ParseTree, xs: Array[Char], n: Int): ParseTree = {
    val banned = "\r\n".toSet

    for (seq <- xs.view
      .iterator
      .filterNot(banned.contains)
      .zipWithIndex
      .sliding(n+1)
      .withPartial(true)) {
      val obs = seq.map(_._1).mkString
      val idxs = seq.map(_._2)
      insertTo(tree, obs, idxs)
    }

    val lastIdx = xs.length - (n + 1)
    for (i <- 0 until n) {
      val idx = lastIdx + i
      val qs = xs.slice(idx, xs.length).filterNot(banned.contains)
      insertTo(tree, qs, idx to xs.length)
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

    tree.getDepth(n).foreach{ _.children = ListBuffer() }
    tree
  }

  // FIXME: sketchy?
  def insertTo(tree: ParseTree, observed: Seq[Char], idx:Seq[Int]): Unit = {

    def go(history: List[Char], active: ParseLeaf, tree: ParseTree, fullHistory: String, idx:Seq[Int]): Unit = {
      if (history.nonEmpty) {
        val maybeNext: Option[ParseLeaf] = active.findChildWithAdditionalHistory(history.head)   // FIXME: sketchy? Note: This is the reverse of expected!!!
        val next = if (maybeNext.isEmpty) active.addChild(history.head) else maybeNext.get
        if (maybeNext.nonEmpty) next.obsCount += 1
        next.addLocation(idx.head)
        go(history.tail, next, tree, fullHistory, idx.tail)
      }
    }

    go(observed.toList, tree.root, tree, observed.mkString, idx)
  }
}

class ParseTree(val alphabet: Alphabet, rootEC: EquivalenceClass=EquivalenceClass()) extends Tree {
  var root:ParseLeaf = new ParseLeaf("", this, rootEC)

  var maxLength:Int = _

  var dataSize:Double = _

  var adjustedDataSize:Double = _

  def navigateHistoryRev(history: String): Option[ParseLeaf] = navigateHistoryRev(history.toList)

  def navigateHistoryRev(history: List[Char]): Option[ParseLeaf] = navigateHistory(history, root, _.head, _.tail)

  def navigateHistory(history: List[Char]): Option[ParseLeaf] = navigateHistory(history, root, _.last, _.init)

  def navigateHistory(history: List[Char], active:ParseLeaf): Option[ParseLeaf] = navigateHistory(history, active, _.last, _.init)

  def navigateHistory(history: List[Char], active:ParseLeaf = root, current:(List[Char])=>Char, prior:(List[Char])=>List[Char])
  : Option[ParseLeaf] = {
    if (history.isEmpty) Option(active) else {
      val maybeNext:Option[ParseLeaf] = active.findChildWithAdditionalHistory(current(history))
      if (prior(history).isEmpty || maybeNext.isEmpty) {
        maybeNext
      } else {
        navigateHistory(prior(history), maybeNext.get)
      }
    }
  }

  def getDepth(depth: Int, nodes:ListBuffer[ParseLeaf] = ListBuffer(root)): Array[ParseLeaf] = {
    if (depth <= 0) {
      nodes.toArray
    } else {
      // [nodes] => [children] ++ [children]
      getDepth(depth-1, nodes.flatMap(_.children))
    }
  }

  def collectLeaves(layer:ListBuffer[ParseLeaf] = ListBuffer(root), collected:ListBuffer[ParseLeaf]=ListBuffer()):Array[ParseLeaf] = {
    if (layer.isEmpty) {
      collected.toArray
    } else {
      val (_, nextLayer) = layer.partition(_.children.isEmpty)
      collectLeaves(nextLayer.flatMap(_.children), collected ++ layer)
    }
  }
}

/**
  * The class representation of a given history.
  *   observedSequence: ABCD
  *   observed        : ABCD
  *   observation     : A
  *
  * @param observedSequence a sequence of observed values.
  * @param parseTree
  * @param initialEquivClass
  **/
class ParseLeaf(observedSequence:String, parseTree: ParseTree, initialEquivClass: EquivalenceClass, parent: Option[ParseLeaf] = None) extends Leaf (parent) {
  var obsCount:Double = 1

  val alphabet:Alphabet = parseTree.alphabet

  val observed:String = observedSequence

  val observation: Char = if ("".equals(observed)) 0.toChar else observed.last // FIXME: REALLY?? THIS IS CORRECT?!?!?!

  val length: Int = observedSequence.length

  var currentEquivalenceClass: EquivalenceClass = initialEquivClass

  val locations:mutable.HashMap[Int, Int] = mutable.HashMap[Int, Int]()

  var children:ListBuffer[ParseLeaf] = ListBuffer()

  @Deprecated
  def incrementDistribution(xNext: Char):Unit = {
    val idx: Int = parseTree.alphabet.map(xNext)
    frequency(idx) += 1
    totalCounts += 1
    distribution = frequency / totalCounts
  }

  def addLocation(idx:Int):Unit = {
    val indexCount = if (locations.keySet.contains(idx)) locations(idx) else 0
    locations += (idx -> (indexCount + 1))
  }

  def calcNextStepProbabilities():Unit = {
    val nextCounts:Array[Double] = alphabet.raw
      .map {
        c => findChildWithAdditionalHistory(c)
          .flatMap{ l => Option(l.obsCount)}
          .getOrElse(0d) }

    frequency = new DenseVector[Double](nextCounts)
    totalCounts = sum(frequency)
    distribution = frequency / totalCounts
  }

  /**
    * Given a "next observation," update the distribution and generate a new child.
    * When loading an observed sequence, "ABC", the final call to addChild (from loadHistory)
    * will have the context of a leaf with the following properties:
    *   - observation 'B'
    *   - observed "BC"
    *
    * the return child will have:
    *   - observation 'A'
    *   - observed "ABC"
    *
    * @param xNext
    * @param dataIdx
    * @return
    */
  def addChild (xNext:Char, dataIdx:Option[Int] = None): ParseLeaf = {
    val maybeNext = findChildWithAdditionalHistory(xNext)
    // FIXME: _TECHNICALLY_ `observed:+xNext` must be reversed if we want this to be correct
    val next:ParseLeaf = if (maybeNext.isEmpty) new ParseLeaf(observed:+xNext, parseTree, currentEquivalenceClass, Option(this)) else maybeNext.get
    if (maybeNext.isEmpty) children += next
    next
  }

  def changeEquivalenceClass(s: EquivalenceClass, paint:Boolean = true): Unit = {
    this.currentEquivalenceClass = s
    // we ought to update transitions here (but for phase II it's not terribly important)
    if (paint) this.children.foreach(_.changeEquivalenceClass(s))
  }

  def findChildWithAdditionalHistory(xNext: Char):Option[ParseLeaf] = {
    // to find BA
    // node A, search for child with B
    children.find(_.observation == xNext)
  }

  def getTransitionState(tree:ParseTree, S:ListBuffer[EquivalenceClass], b:Char):Option[EquivalenceClass] = {
    val navigatableHistory = if (observed.length == tree.maxLength) (observed + b).tail else observed + b

    tree
      .navigateHistoryRev(navigatableHistory.toList)
      .flatMap{ l => Option(l.currentEquivalenceClass) }
      .filter{ S.contains(_) }
  }

  @Deprecated
  def getLoopingStateOnTransitionTo(tree:ParseTree, b:Char):Option[EquivalenceClass] = {
    val isLast:Boolean = this.observed.length == tree.maxLength && parent.nonEmpty
    val navigatableHistory = if (isLast) b + parent.get.observed else b + observed

    tree
      .navigateHistory(navigatableHistory.toList)
      .flatMap{ l => Option(l.currentEquivalenceClass) }
  }

  @Deprecated
  def getStateOnTransitionTo(b:Char):Option[EquivalenceClass] = {
    val optionalLeaf = parseTree.navigateHistory((b + observed).toList)
    if (optionalLeaf.nonEmpty) Option(optionalLeaf.get.currentEquivalenceClass) else None
  }

  def fullString: String = {
    val vec = distribution.toArray.mkString("(", ", ", ")")
    val id = s"${getClass.getSimpleName}@${hashCode()}"
    val nTabs = if (observed.length < 4) 3 else 2

    val props = s"{dist=$vec,\tobserved=$observed, \tobservation=${observation.toString},\ttotal=${sum(frequency)}}"
    observed + "\t" * nTabs + id + "\t" + props
  }

  def shortString: String = observed

  override def toString: String = fullString
}

