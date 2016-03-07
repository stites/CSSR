package com.typeclassified.hmm.cssr.trees

import breeze.linalg.{sum, DenseVector}
import breeze.numerics.{rint, round}
import com.typeclassified.hmm.cssr.parse.Alphabet
import com.typeclassified.hmm.cssr.shared.Epsilon

import scala.collection.mutable.ListBuffer

object LoopingTree {
  /*
  def from(tree: ParseTree):LoopingTree = {
    var n:Integer = 0
    val ltree = new LoopingTree(tree.alphabet)
    ltree.root.add(tree.root)

    ltree.walk(lleaf => {
      val nonHomog = lleaf
        .histories
        .filterNot(LoopingTree.homogeneous(tree, _))
      // what if we don't remove them and, instead, we keep them there for historical purposes

      val children = nonHomog
        .flatMap{ _.children }
        .groupBy{ _.observation }
        .map{ case (c, hs) => c -> new LoopingLeaf(c, ltree, hs, Option(lleaf)) }

      lleaf.exhausted = nonHomog.length == lleaf.histories.length
      lleaf.children ++= children
    })


    ltree.walk(lleaf => {
      val nonHomog = lleaf
        .histories
        .filterNot(LoopingTree.homogeneous(tree, _))
      // what if we don't remove them and, instead, we keep them there for historical purposes

      val children = nonHomog
        .flatMap{ _.children }
        .groupBy{ _.observation }
        .map{ case (c, hs) => c -> new LoopingLeaf(c, ltree, hs, Option(lleaf)) }

      lleaf.exhausted = nonHomog.length == lleaf.histories.length
      lleaf.children ++= children
    })


    ltree
  }
  */

  def allHomogeneous(tree: ParseTree, w:ParseLeaf): Boolean = homogeneous(allPrefixes(tree, w), w)
  def nextHomogeneous(tree: ParseTree, w:ParseLeaf): Boolean = homogeneous(nextPrefixes(tree, w), w)

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

}

class LoopingTree (val alphabet:Alphabet) extends Tree {
  var root:LoopingLeaf = new LoopingLeaf('\0', this)

  def this(ptree: ParseTree) = {
    this(ptree.alphabet)
    this.root = new LoopingLeaf(ptree.root.observation, this)
  }

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

  def walk(visitor:(LoopingLeaf)=>Unit, active:LoopingLeaf = root):Unit = {
    if (!active.exhausted) {
      visitor(active)
    }

    active
      .children
      .mapValues{ a => {
        walk(visitor, a)
      } }
  }
}


class LoopingLeaf (val observation:Char,
                   val tree:LoopingTree,
                   val histories:ListBuffer[ParseLeaf] = ListBuffer(),
                   parent:Option[LoopingLeaf] = None
) extends Leaf (parent) {
  if (histories.nonEmpty) this.distribution = histories.head.distribution

  var children:Map[Char, LoopingLeaf] = Map()

  var rounded:DenseVector[Double] = if (sum(distribution) > 0) Tree.round(distribution) else distribution

  var exhausted:Boolean = false

  var loop:Option[LoopingLeaf] = None

  def this(p: ParseLeaf, tree:LoopingTree) = this(p.observation, tree, ListBuffer(p), None)

  def ++=(lLeaf: LoopingLeaf) = {
    if (Tree.matches(this)(lLeaf)) this.histories ++= lLeaf.histories
  }

  def add(history: ParseLeaf) = {
    if (histories.isEmpty) {
      histories += history
      distribution = history.distribution
    } else {
      throw new RuntimeException("don't invoke this on an empty Node")
    }
  }

  def inspectChildren() = {
    add(this.histories.flatMap{_.children}.toArray)
  }

  def add(histories: Array[ParseLeaf]) = {
    if (this.histories.isEmpty) throw new RuntimeException("yeah... this shouldn't happen")
    splitAndAddChildren(histories)
  }

  def splitAndAddChildren(histories:Array[ParseLeaf])(implicit ep:Epsilon):Unit = {
    val grouped = histories.to[ListBuffer]
      .groupBy( history => Tree.round(history) )

    if (grouped.keySet.contains(Tree.round(this))) {
      this.histories ++= grouped(Tree.round(distribution))
    }

    val nextActiveNodes:Map[Char, LoopingLeaf] = grouped
      .filterKeys{ !Tree.matches(distribution)(_) }
      .flatMap {
        case (groupedDist, histsByDist) => {
          // FIXME: handle differences in strings of >1 step of history:
          // eg:     lnode has ["", a, b, ab, aa, bb, ba]. We encounter "abb"... what if we
          //         encounter "abbb"?
          // answer: I don't think this error will ever happen, but in the case of "abb"
          //         followed by "abbb" we will see: ["", a, b, ab, aa, bb, ba] =b=> [abb] =b=> [abbb]
          //
          // FIXME: handle children that have the same observed, but different distributions:
          // eg: lnode has ["", a, b]. We encounter "ab" now
          //           {hs:["", a, b], d:[0.5,0.5]} --b--> {hs:[ab], d:[0.2,0.8]}
          //
          histsByDist
            .groupBy(_.observation)
            .map { case (c, hs) => c -> new LoopingLeaf(c, tree, hs, Some(this))}
        } }

    val x = nextActiveNodes
      .foldLeft(Map[Char, LoopingLeaf]()){
        case (nextNodes, (c, loopingLeaf)) => {
          val d = Tree.round(loopingLeaf)
          val ll = tree.collectLeaves()
          val leaf:Option[LoopingLeaf] = ll.find(Tree.matches(loopingLeaf))
          val ds = ll.map(Tree.round(_))

          if (leaf.nonEmpty) {
            this ++= leaf.get
            nextNodes
          } else {
            nextNodes + (c -> loopingLeaf)
          }
        }
      }

    children ++= x
  }
}

