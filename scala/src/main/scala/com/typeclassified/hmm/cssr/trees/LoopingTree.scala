package com.typeclassified.hmm.cssr.trees

import breeze.linalg.DenseVector
import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, Alphabet}
import com.typeclassified.hmm.cssr.shared.Probablistic

import scala.collection.mutable.ListBuffer

object LoopingTree {
  def from(tree: ParseTree):LoopingTree = {
    var n:Integer = 0
    val ltree = new LoopingTree(tree)

    n = 1
    var histories = tree.getDepth(n)
    var terminals = ListBuffer(ltree.root)

    terminals
      .foreach {
        active => {
          val first = active.distribution
          val additions:ListBuffer[ParseLeaf] = ListBuffer()
          val grouped = histories.groupBy { _.distribution }
          grouped.foreach {
            case (dist, hs) =>
              if (dist == active.histories.head.distribution) {
                active.histories ++= hs
              } else {
                newTerminals += new LoopingLeaf(hs.to[ListBuffer])
              }
          } } }
    terminals ++= newTerminals

    n = 2
    histories = tree.getDepth(n)
    terminals = ltree.collectLeaves()

    newTerminals = ListBuffer()
    terminals
      .foreach {
        active => {
          val first = active.histories.head
          val additions:ListBuffer[ParseLeaf] = ListBuffer()
          val grouped = histories.groupBy { _.distribution }
          grouped.foreach {
            case (dist, hs) =>
              if (dist == active.histories.head.distribution) {
                active.histories ++= hs
              } else {
                newTerminals += new LoopingLeaf(hs.to[ListBuffer])
              }
          } } }
    terminals ++= newTerminals

    ltree
  }

  def matches(u:Probablistic, w:Probablistic): Boolean = u.distribution == w.distribution

  def homogeneous(allHistories:ListBuffer[ParseLeaf], w:ParseLeaf): Boolean = allHistories.forall { pw => matches(pw, w) }

  def homogeneous(tree: ParseTree, w:ParseLeaf): Boolean = homogeneous(prefixes(tree, w), w)

  def excisable(tree:ParseTree)(w:ParseLeaf, e:String):Boolean = excisable(tree, prefixes(tree, w))(w, e)

  def excisable(tree:ParseTree, uews: ListBuffer[ParseLeaf])(w:ParseLeaf, e:String):Boolean = {
    excise(tree, uews)(w, e)
      .map {
        case (uew, Some(uw)) => LoopingTree.matches(uew, uw)
        case (uew, None)     => true
        case _               => true
      }
      .forall(isExcisable => isExcisable)
  }

  def excise(tree:ParseTree, uews: ListBuffer[ParseLeaf])(w:ParseLeaf, e:String)
  :ListBuffer[(ParseLeaf, Option[ParseLeaf])] = {
    val exciseStr:(String)=>String = excise(w.observed, e)

    uews
      .map { uew => (uew, exciseStr(uew.observed)) }
      .map { case (uew, uwStr) => (uew, tree.navigateHistoryRev(uwStr)) }
  }

  def excise(w:String, e:String)(uew: String):String = uew.take(uew.length - e.length - w.length) + w

  def prefixes(histories: ListBuffer[ParseLeaf], w:String):ListBuffer[ParseLeaf] = histories
    .filter { _.observed.takeRight(w.length) == w }

  def prefixes(tree: ParseTree, w:String):ListBuffer[ParseLeaf] = {
    val histories = (w.length to tree.maxLength)
      .flatMap { n => tree.getDepth(n) }
      .to[ListBuffer]

    prefixes(histories, w)
  }

  def prefixes(histories: ListBuffer[ParseLeaf], w:ParseLeaf):ListBuffer[ParseLeaf] = prefixes(histories, w.observed)

  def prefixes(tree: ParseTree, w:ParseLeaf):ListBuffer[ParseLeaf] = prefixes(tree, w.observed)
}

class LoopingTree (
  val alphabet:Alphabet,
  val root:LoopingLeaf = new LoopingLeaf()
) {
  def this(ptree: ParseTree) = this(ptree.alphabet, new LoopingLeaf(ptree.root))

  def getDepth(depth: Int, nodes:ListBuffer[LoopingLeaf] = ListBuffer(root)): Array[LoopingLeaf] = {
    if (depth <= 0) nodes.toArray else getDepth(depth-1, nodes.flatMap(_.children.values.to[ListBuffer]))
  }

  def collectLeaves(
    layer:ListBuffer[LoopingLeaf] = ListBuffer(root),
    collected:ListBuffer[LoopingLeaf]=ListBuffer()
  ):Array[LoopingLeaf] = {
    if (layer.isEmpty) collected.toArray else {
      val nextLayer = layer.partition(_.children.isEmpty)._2
      collectLeaves(nextLayer.flatMap(_.children.values.to[ListBuffer]), collected ++ layer)
    }
  }
}


class LoopingLeaf ( val observation:Char,
                    val histories:ListBuffer[ParseLeaf] = ListBuffer(),
                    val parent:Option[LoopingLeaf] = None,
                    override var distribution:DenseVector[Double] = DenseVector.zeros(AlphabetHolder.alphabet.length)
) extends Probablistic {
  var children:Map[Char, LoopingLeaf] = Map()

  def this() = this('\0')

  def this(char: Char) = this(char)

  def this(p: ParseLeaf) = this(p.observation, ListBuffer(p), None, p.distribution)

  def this(ps: ListBuffer[ParseLeaf]) = this('\0', ps, None, ps.head.distribution)

  def this(ps: ListBuffer[ParseLeaf], parent:Option[LoopingLeaf]) = this('\0', ps, parent, ps.head.distribution)

  def this(char: Char, ps: ListBuffer[ParseLeaf]) = this(char, ps, None, ps.head.distribution)

  def add(history: ParseLeaf) = {
    if (histories.isEmpty) distribution = history.distribution
    if (LoopingTree.matches(this, history)) histories += history
  }

  def add(histories: Array[ParseLeaf]) = {
    if (this.histories.isEmpty) distribution = histories.head.distribution
    splitAndAddChildren(distribution, histories)
  }

  def splitAndAddChildren(dist:DenseVector[Double], histories:Array[ParseLeaf]):Unit = {
    val grouped = histories.to[ListBuffer].groupBy { _.distribution }

    if (grouped.keySet.contains(distribution)) {
      this.histories ++= grouped(distribution)
    }

    val nextActiveNodes:Map[Char, LoopingLeaf] = grouped
      .filterKeys{ _ != distribution }
      .flatMap {
        case (groupedDist, histsByDist) => {
          // FIXME: handle differences in strings of >1 step of history:
          // eg: lnode has ["", a, b, ab, aa, bb, ba]. We encounter "abb"... what if we encounter "abbb"?
          // answer — I don't think this error will ever happen, but in the case of "abb" followed by "abbb" we will see:
          // ["", a, b, ab, aa, bb, ba] =b=> [abb] =b=> [abbb]

          // FIXME: handle childern that have the same observed, but different distributions:
          // eg: lnode has ["", a, b]. We encounter "ab" now {hs=["", a, b], dist=[0.5,0.5]} =b=> {hs=[ab], dist=[0.2,0.8]}
          //     next we come across an abb... this will never happen
          histsByDist
            .groupBy(_.observation)
            .map { case (c, hs) => c -> new LoopingLeaf(c, hs, Some(this), hs.head.distribution)}
        } }

    children ++= nextActiveNodes
  }
}

