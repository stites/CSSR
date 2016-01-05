package com.typeclassified.hmm.cssr.parse

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
class Leaf(observedSequence:String,
           parseTree: Tree,
           initialEquivClass: EquivalenceClass,
           var parent: Option[Leaf] = None
          ) extends Probablistic {
  val alphabet:Alphabet = parseTree.alphabet

  val observed:String = observedSequence

  val observation: Char = if ("".equals(observed)) 0.toChar else observed.head

  var currentEquivalenceClass: EquivalenceClass = initialEquivClass

  val locations:mutable.HashMap[Int, Int] = mutable.HashMap[Int, Int]()

  var children: ListBuffer[Leaf] = ListBuffer()

  def printParent (x:Option[Leaf] = Some(this), acc:String = "") :String = {
    if (x.isEmpty) {
      acc
    } else {
      printParent(x.get.parent, acc + x.get.observation)
    }
  }

  def updateDistribution(xNext: Char, dataIdx:Option[Int] = None):Unit = {
    val idx: Int = parseTree.alphabet.map(xNext)

    frequency(idx) += 1
    totalCounts += 1
    distribution = frequency / totalCounts

    if (dataIdx.nonEmpty) {
      val indexCount = if (locations.keySet.contains(dataIdx.get)) locations(dataIdx.get) else 0
      locations += (dataIdx.get -> (indexCount + 1))
    }
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
    updateDistribution(xNext, dataIdx)
    val maybeNext = findChildWithAdditionalHistory(xNext)
    var next:Leaf = null
    if (maybeNext.isEmpty) {
      next = new Leaf(xNext+:observed, parseTree, currentEquivalenceClass, Some(this))
      children += next
    } else {
      next = maybeNext.get
    }
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

  def getStateOnTransitionTo(b:Char):Option[EquivalenceClass] = {
    val optionalLeaf = parseTree.navigateHistory((b + observed).toList)
    if (optionalLeaf.nonEmpty) Some(optionalLeaf.get.currentEquivalenceClass) else None
  }

  override def toString: String = {
    val vec = frequency.toArray.mkString("(", ", ", ")")
    val id = s"${getClass.getSimpleName}@${this.hashCode()}"

    val props = s"{observed=${observed}, \tobservation=${observation},\tfrequency=${vec}}"
    id + "\t" + props
  }
}

