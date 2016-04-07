package com.typeclassified.hmm.cssr.trees

import breeze.linalg.{DenseVector, sum}
import com.typeclassified.hmm.cssr.parse.Alphabet
import com.typeclassified.hmm.cssr.shared.Logging
import com.typeclassified.hmm.cssr.state.EquivalenceClass

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/*
We encounter the history say "110"
We go to the parse tree at the root
We take the 0 child of the root
We then take the 1 child of 0 (= 10)
We then take the 1 child of 10 (=110)
*/
object ParseTree extends Logging {
  def apply(alphabet: Alphabet, equivalenceClass: EquivalenceClass) = new ParseTree(alphabet, equivalenceClass)
  def apply(alphabet: Alphabet) = new ParseTree(alphabet)

  def loadData(tree:ParseTree, xs: Array[Char], n: Int): ParseTree = {
    val banned = "\r\n".toSet
    // terrible for something we can probably fuse into the following:
    val filteredCharactersCount = xs.count(banned.contains)
    tree.maxLength = n
    tree.dataSize = xs.length - filteredCharactersCount
    tree.adjustedDataSize =  xs.length - filteredCharactersCount
    val checkpoints:Set[Double] = (1 until 4).map(i => tree.dataSize * i / 4 ).toSet

    for (seq <- xs.view
      .iterator
      .filterNot(banned.contains)
      .zipWithIndex
      .sliding(n+1)
      .withPartial(false)) {
      val obs = seq.map(_._1).mkString
      val idxs = seq.map(_._2)
      if (checkpoints.contains(idxs.head)) {
        info(s"${idxs.head / tree.dataSize * 100}% of data streamed")
      }
      cleanInsert(tree, obs, idxs)
    }

    // mostly for test cases.
    val last:Int = if (n > tree.adjustedDataSize) tree.adjustedDataSize.toInt else n

    for (i <- (0 to last).reverse) {
      val left = xs.take(i).filterNot(banned.contains)
      val lIdxs = 0 until i
      cleanInsert(tree, left , lIdxs )
    }

    info("data streaming complete")

    // calculate conditional histories
    for (depth <- 0 to n) {
      tree.getDepth(depth).foreach(_.calcNextStepProbabilities(tree))
    }

    // remove the final depth so that we are left only with predictive distributions. Note that this isn't strictly necessary.
    tree.getDepth(n).foreach{ _.children = ListBuffer() }
    tree
  }

  def getCurrent[A](observed:Iterable[A]) = observed.last
  def getPrior[A](observed:Iterable[A]) = observed.init

  type CurrentHistory = (Char, Int)
  type OlderHistory = (Iterable[Char], Iterable[Int])

  protected def splitHistoryClean (observed: Iterable[Char], idx:Iterable[Int]): (CurrentHistory , OlderHistory) = {
    ((getCurrent(observed), getCurrent(idx)), (getPrior(observed), getPrior(idx)))
  }

  type GetCurrent[A >: AnyVal] = (List[A])=>A
  type GetPrior[A >: AnyVal] = (List[A])=>List[A]

  def cleanInsert(tree: ParseTree, observed: Iterable[Char], idx:Iterable[Int]): Unit = {
    def go(history: Iterable[Char], active: ParseLeaf, tree: ParseTree, fullHistory: String, idx:Iterable[Int]): Unit = {
      if (history.nonEmpty) {
        val ((current, cIdx), (older, oIdx)) = splitHistoryClean(history, idx)
        val maybeNext: Option[ParseLeaf] = active.next(current)
        val next = if (maybeNext.isEmpty) active.addChild(current) else maybeNext.get
        if (maybeNext.nonEmpty) next.obsCount += 1
        next.addLocation(cIdx)
        go(older, next, tree, fullHistory, oIdx)
      }
    }
    go(observed.toList, tree.root, tree, observed.mkString, idx)
  }

}

class ParseTree(val alphabet: Alphabet, rootEC: EquivalenceClass=EquivalenceClass()) extends Tree[ParseLeaf](new ParseLeaf("", rootEC)) {
  var maxLength:Int = _

  var dataSize:Double = _

  var adjustedDataSize:Double = _

  def navigateHistoryRev(history: Iterable[Char]): Option[ParseLeaf] = navigateHistory(history, root, _.head, _.tail)
}

/**
  * The class representation of a given history.
  *   observedSequence: ABCD
  *   observed        : ABCD
  *   observation     : A
  *
  * @param observed a sequence of observed values.
  **/
class ParseLeaf(val observed:String, initialEquivClass: EquivalenceClass, parent: Option[ParseLeaf] = None) extends Leaf[ParseLeaf] (if ("".equals(observed)) 0.toChar else observed.head, parent) {
  var obsCount:Double = 1

  val length: Int = observed.length

  var currentEquivalenceClass: EquivalenceClass = initialEquivClass

  val locations:mutable.HashMap[Int, Int] = mutable.HashMap[Int, Int]()

  var children:ListBuffer[ParseLeaf] = ListBuffer()

  override def getChildren(): Iterable[ParseLeaf] = children

  def addLocation(idx:Int):Unit = {
    val indexCount = if (locations.keySet.contains(idx)) locations(idx) else 0
    locations += (idx -> (indexCount + 1))
  }

  def calcNextStepProbabilities(parseTree: ParseTree):Unit = {
    val nextCounts:Array[Double] = parseTree.alphabet.raw
      .map {
        c => parseTree.navigateHistory(observed + c)
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
    */
  def addChild (xNext:Char, dataIdx:Option[Int] = None): ParseLeaf = {
    val maybeNext = this.next(xNext)
    val next:ParseLeaf = if (maybeNext.isEmpty) new ParseLeaf(xNext +: observed, currentEquivalenceClass, Option(this)) else maybeNext.get
    if (maybeNext.isEmpty) children += next
    next
  }

  /** to find node BAC, we traverse the tree from NULL -> B -> A -> C. Thus, from node BA, we search for C. */
  override def next(xNext: Char):Option[ParseLeaf] = children.find(_.observation == xNext)

  def fullString: String = {
    val vec = distribution.toArray.mkString("(", ", ", ")")
    val id = s"${getClass.getSimpleName}@${hashCode()}"
    val nTabs = if (observed.length < 4) 3 else 2

    val props = s"{dist=$vec,\tobservation=${observation.toString},\ttotal=${sum(frequency)}}"
    observed + "\t" * 1 + id + "\t" + props
  }

  def getTransitionState(tree:ParseTree, S:ListBuffer[EquivalenceClass], b:Char):Option[EquivalenceClass] = {
    val navigatableHistory = if (observed.length == tree.maxLength) (observed + b).tail else observed + b

    tree
      .navigateHistoryRev(navigatableHistory.toList)
      .flatMap{ l => Option(l.currentEquivalenceClass) }
      .filter{ S.contains(_) }
  }

  def changeEquivalenceClass(s: EquivalenceClass, paint:Boolean = true): Unit = {
    this.currentEquivalenceClass = s
    if (paint) this.children.foreach(_.changeEquivalenceClass(s))
  }

  def shortString: String = observed

  override def toString: String = fullString
}

