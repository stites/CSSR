package com.typeclassified.hmm.cssr.parse

import breeze.linalg.{DenseVector, sum}
import com.typeclassified.hmm.cssr.shared.Probablistic
import com.typeclassified.hmm.cssr.state.EquivalenceClass

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
class Leaf(observedSequence:String, parseTree: Tree, initialEquivClass: EquivalenceClass, var parent: Option[Leaf] = None ) extends Probablistic {
  var obsCount:Double = 1

  val alphabet:Alphabet = parseTree.alphabet

  val observed:String = observedSequence

  val observation: Char = if ("".equals(observed)) 0.toChar else observed.last // FIXME: REALLY?? THIS IS CORRECT?!?!?!

  val length: Int = observedSequence.length

  var currentEquivalenceClass: EquivalenceClass = initialEquivClass

  val locations:mutable.HashMap[Int, Int] = mutable.HashMap[Int, Int]()

  var children:ListBuffer[Leaf] = ListBuffer()

  def printParent (x:Option[Leaf] = Option(this), acc:String = "") :String = {
    if (x.isEmpty) acc else printParent(x.get.parent, acc + x.get.observation)
  }

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
  def addChild (xNext:Char, dataIdx:Option[Int] = None): Leaf = {
    val maybeNext = findChildWithAdditionalHistory(xNext)
    // FIXME: _TECHNICALLY_ `observed:+xNext` must be reversed if we want this to be correct
    val next:Leaf = if (maybeNext.isEmpty) new Leaf(observed:+xNext, parseTree, currentEquivalenceClass, Option(this)) else maybeNext.get
    if (maybeNext.isEmpty) children += next
    next
  }

  def changeEquivalenceClass(s: EquivalenceClass): Unit = {
    this.currentEquivalenceClass = s
    // we ought to update transitions here (but for phase II it's not terribly important)
    this.children.foreach(_.changeEquivalenceClass(s))
  }

  def findChildWithAdditionalHistory(xNext: Char):Option[Leaf] = {
    // to find BA
    // node A, search for child with B
    children.find(_.observation == xNext)
  }

  def getRevLoopingStateOnTransitionTo(tree:Tree, S:ListBuffer[EquivalenceClass], b:Char):Option[EquivalenceClass] = {
    val navigatableHistory = if (observed.length == tree.maxLength) (observed + b).tail else observed + b

    tree
      .navigateHistoryRev(navigatableHistory.toList)
      .flatMap{ l => Option(l.currentEquivalenceClass) }
      .filter{ S.contains(_) }
  }

  @Deprecated
  def getLoopingStateOnTransitionTo(tree:Tree, b:Char):Option[EquivalenceClass] = {
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

  override def toString: String = {
    val vec = frequency.toArray.mkString("(", ", ", ")")
    val id = s"${getClass.getSimpleName}@${hashCode()}"
    val nTabs = if (observed.length < 4) 3 else 2

    val props = s"{observed=$observed, \tobservation=${observation.toString},\tfrequency=$vec,\ttotal=${sum(frequency)}}"
    observed + "\t" * nTabs + id + "\t" + props
  }
}

