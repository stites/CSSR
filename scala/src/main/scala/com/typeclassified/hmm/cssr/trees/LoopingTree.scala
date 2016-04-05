package com.typeclassified.hmm.cssr.trees

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.hmm.cssr.parse.Alphabet

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

  type Node = Either[LLeaf, Loop]
}

class LoopingTree (val alphabet:Alphabet) extends Tree {
  var root:LLeaf = _

  val terminals:mutable.Set[LLeaf] = mutable.HashSet()

  def this(ptree: ParseTree) = {
    this(ptree.alphabet)
    root = new LLeaf(ptree.root.observation, this, ListBuffer(ptree.root))
    terminals.add(root)
  }

  def getDepth(depth: Int, nodes:Iterable[LLeaf] = ListBuffer(root)): Array[LLeaf] = {
    if (depth <= 0) nodes.toArray else getDepth(depth-1, nodes.flatMap(_.leafChildren()))
  }

  def collectLeaves( layer:ListBuffer[LLeaf] = ListBuffer(root), collected:Iterable[LLeaf]=ListBuffer() ):Array[LLeaf] = {
    if (layer.isEmpty) collected.toArray else {
      val nextLayer = layer.partition(_.children.isEmpty)._2
      collectLeaves(nextLayer.flatMap(_.leafChildren()), collected ++ layer)
    }
  }
}

class Loop (val loop: LLeaf) {
  // a simple marker class
  override def toString: String = getClass.getSimpleName + "(" + loop.toString() + ")"
}


class LLeaf(val observation:Char,
            val tree:LoopingTree,
            val histories:ListBuffer[ParseLeaf] = ListBuffer(),
            parent:Option[LLeaf] = None
) extends Leaf[LLeaf] (parent) {
  if (histories.nonEmpty) this.distribution = histories.head.distribution

  var children:mutable.Map[Char, LoopingTree.Node] = mutable.Map()

  var rounded:DenseVector[Double] = if (sum(distribution) > 0) Tree.round(distribution) else distribution

  var exhausted:Boolean = false

  def this(p: ParseLeaf, tree:LoopingTree) = this(p.observation, tree, ListBuffer(p), None)

  def ++=(lLeaf: LLeaf) = {
    if (Tree.matches(this)(lLeaf)) this.histories ++= lLeaf.histories
  }

  def leafChildren():Iterable[LLeaf] = LoopingTree.leafChildren(children)

  override def toString():String = {
    val hists = histories.map(_.observed).mkString("[", ",", "]")
    val dist = rounded.toArray.mkString("[", ",", "]")
    s"{rounded:$dist, size: ${histories.size}, histories: $hists}"
  }
}

