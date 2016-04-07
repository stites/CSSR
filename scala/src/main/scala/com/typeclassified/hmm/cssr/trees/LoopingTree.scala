package com.typeclassified.hmm.cssr.trees

import com.typeclassified.hmm.cssr.parse.Alphabet

import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object LoopingTree {
  def allHomogeneous(tree: ParseTree, w:ParseLeaf): Boolean = homogeneous(allPrefixes(tree, w), w)
  def nextHomogeneous(tree: ParseTree)(w:ParseLeaf): Boolean = homogeneous(nextPrefixes(tree, w), w)

  def homogeneous(allHistories:ListBuffer[ParseLeaf], w:ParseLeaf): Boolean = allHistories.forall { pw => Tree.matches(w)(pw) }

  def allExcisable(tree:ParseTree)(w:ParseLeaf, e:String):Boolean = excisable(tree, allPrefixes(tree, w))(w, e)
  def nextExcisable(tree:ParseTree)(w:ParseLeaf, e:String):Boolean = excisable(tree, nextPrefixes(tree, w))(w, e)

  def excisable(tree:ParseTree, uews: ListBuffer[ParseLeaf])(w:ParseLeaf, e:String):Boolean = {
    excise(tree, uews)(w, e)
      .forall {
        case (uew, Some(uw)) => Tree.matches(uew)(uw)
        case (uew, None)     => true
        case _               => true
      }
  }

  def excise(tree:ParseTree, uews: ListBuffer[ParseLeaf])(w:ParseLeaf, e:String):ListBuffer[(ParseLeaf, Option[ParseLeaf])] = {
    val exciseStr:(String)=>String = excise(w.observed, e)
    uews
      .map { uew => (uew, exciseStr(uew.observed)) }
      .map { case (uew, uwStr) => (uew, tree.navigateHistoryRev(uwStr)) }
  }

  def excise(w:String, e:String)(uew: String):String = uew.take(uew.length - e.length - w.length) + w

  def prefixes(histories: ListBuffer[ParseLeaf], w:String):ListBuffer[ParseLeaf] = histories
    .filter { _.observed.takeRight(w.length) == w }

  def prefixes(histories: ListBuffer[ParseLeaf], w:ParseLeaf):ListBuffer[ParseLeaf] = prefixes(histories, w.observed)

  def prefixes(tree: ParseTree, w:String, prefixDepth:Int):ListBuffer[ParseLeaf] = {
    val histories = (w.length to prefixDepth)
      .flatMap { n => tree.getDepth(n) }
      .to[ListBuffer]

    prefixes(histories, w)
  }

  def nextPrefixes(tree: ParseTree, w:ParseLeaf):ListBuffer[ParseLeaf] = nextPrefixes(tree, w.observed)
  def nextPrefixes(tree: ParseTree, w:String):ListBuffer[ParseLeaf] = prefixes(tree, w, w.length+1)

  def allPrefixes(tree: ParseTree, w:ParseLeaf):ListBuffer[ParseLeaf] = allPrefixes(tree, w.observed)
  def allPrefixes(tree: ParseTree, w:String):ListBuffer[ParseLeaf] = prefixes(tree, w, tree.maxLength)

  def leafChildren(lleaves:Iterable[(Char, Node)]):Iterable[LLeaf] = lleaves.toMap.values.foldLeft(List[LLeaf]()){
    case (list, Left(a)) => list :+ a
    case (list, Right(a)) => list
  }

  type AltNode = Either[Loop, EdgeSet]
  type Node = Either[LLeaf, AltNode]

  def findLoop(lleaf:LLeaf):Option[Loop] = Tree.firstExcisable(lleaf).flatMap( l => Some(new Loop(l)) )

  def findEdgeSet(ltree: LoopingTree, lleaf: LLeaf):Option[EdgeSet] = {
    val terminals = ltree.terminals -- Tree.getAncestorsRecursive(lleaf).toSet[LLeaf]
    val terminalMatches:Set[LLeaf] = terminals.filter(Tree.matches(lleaf))
    if (terminalMatches.isEmpty) {
      None
    } else {
      val found:Option[EdgeSet] = terminalMatches
        .find{ _.edgeSet.isDefined }
        .flatMap{ _.edgeSet }

      if (found.nonEmpty) {
        val foundEdges = terminalMatches.filter(_.edgeSet.isDefined).map(_.edgeSet.get)
        assert(foundEdges.forall{ _ == found.get }, "All found edges must be the same.")
      }

      Option.apply(found.getOrElse( new EdgeSet(lleaf, terminalMatches) ))
    }
  }

  def findAlt(loopingTree: LoopingTree)(lleaf: LLeaf):Option[AltNode] = {
    lazy val edgeSet = findEdgeSet(loopingTree, lleaf)

    LoopingTree.findLoop(lleaf) match {
      case Some(l) => Some(Left(l))
      case None => if (edgeSet.isEmpty) None else Some(Right(edgeSet.get))
    }
  }

  def getLeaf(node:LoopingTree.Node):LLeaf = node match {
    case Left(lnode) => lnode
    case Right(Right(edgeset)) => edgeset.value
    case Right(Left(loop)) => loop.value
  }
}

class LoopingTree(val alphabet:Alphabet, root:LLeaf) extends Tree[LLeaf](root) {
  /** the set of non-looping terminal nodes */
  var terminals:Set[LLeaf] = HashSet()

  def this(ptree: ParseTree) = {
    this(ptree.alphabet, new LLeaf(ptree.root.observation, List(ptree.root)))
    terminals = terminals + root
  }

  def navigateLoopingPath(history: Iterable[Char]):Option[LoopingTree.Node] = navigateLoopingPath(history, Some(Left(root)), _.head, _.tail)

  def navigateLoopingPath(history: Iterable[Char], active:Option[LoopingTree.Node], current:(Iterable[Char])=>Char, prior:(Iterable[Char])=>Iterable[Char]): Option[LoopingTree.Node] = {
    if (history.isEmpty) active else {
      val next = active.flatMap { node => LoopingTree.getLeaf(node).nextLeaf(current(history)) }
      navigateLoopingPath(prior(history), next, current, prior)
    }
  }
}

// a simple abstract marker class for a looping tree's Loops and Edges
abstract class LoopWrapper(val value:LLeaf) {
  override def toString: String = getClass.getSimpleName + "(" + value.toString() + ")"
}

class Loop (loop: LLeaf) extends LoopWrapper(loop) {
}

class EdgeSet (edge: LLeaf, val edges:Set[LLeaf]) extends LoopWrapper(edge) {

}

class LLeaf(observation:Char, var histories:List[ParseLeaf] = List(), parent:Option[LLeaf] = None) extends Leaf[LLeaf] (observation, parent) {
  recalculate(histories)

  var children:mutable.Map[Char, LoopingTree.Node] = mutable.Map()

  var edgeSet:Option[EdgeSet] = None

  def this(p: ParseLeaf) = this(p.observation, List(p), None)

  def ++=(lLeaf: LLeaf) = if (Tree.matches(this)(lLeaf)) this.histories ++= lLeaf.histories

  override def getChildren():Iterable[LLeaf] = LoopingTree.leafChildren(children)

  def nextLeaf(c: Char): Option[LoopingTree.Node] = children.get(c)

  // this duplicates LoopingTree.getLeaf
  override def next(c: Char): Option[LLeaf] = children.get(c).flatMap {
    case Left(lleaf) => Option(lleaf)
    case _ => None
  }

  def addHistories(newHistories:ListBuffer[ParseLeaf]):Unit = {
    histories ++= newHistories
    recalculate(histories)
  }

  override def toString():String = {
    val hists = histories.map(_.observed).mkString("[", ",", "]")
    val dist = rounded.toArray.mkString("[", ",", "]")
    s"{rounded:$dist, size: ${histories.size}, histories: $hists}"
  }
}

