package com.typeclassified.hmm.cssr.trees

import com.typeclassified.hmm.cssr.shared.{Epsilon, Probablistic}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

object Tree {
  implicit val ep:Epsilon = new Epsilon(0.01)
  implicit val sig:Double = 0.001d

  /**
    * Two words match if and only if they lead to the same prediction for the next step.
    *
    * @param u A leaf node from a parse tree, representing a word
    * @param w A leaf node from a parse tree, representing a word
    * @return whether or not the two leaves have the same prediction for the next step
    */
  def matches[L1 <: Probablistic, L2 <: Probablistic](u:L1)(w:L2): Boolean = u.testNull(w)

  /**
    * Given some word {@code w}, and word {@code eq}, where w = eq: we call prefix e
    * excisable from w iff, for all prefixes p, match(peq, pq). That is, inserting the
    * extra history e before q makes no difference to predictions.
    *
    * @param w A leaf node from a parse tree, representing a word
    * @return the first possible ancestor leaf which has the same next-step prediction as w
    */
  def firstExcisable[L <: Leaf[L]](w:L):Option[L] = {
    // ancestors must be ordered by depth with the root first. Hence, reverse
    getAncestors(w).reverse.find{matches(w)}
  }

  def getAncestors[L <: Leaf[L]](w:L):List[L] = {
    val ancestors = ListBuffer[L]()
    var active = w
    while (active.parent.nonEmpty) {
      ancestors += active.parent.get
      active = active.parent.get
    }
    ancestors.toList
  }

  def getAncestorsRecursive[L <: Leaf[L]](w:L, ancestors:List[L]=List()):List[L] = {
    val active = Option.apply(w)
    if (active.isEmpty) {
      ancestors
    } else {
      val next:List[L] = ancestors ++ List(active).flatten
      val parent:Option[L] = active.flatMap(_.parent)
      if (parent.isEmpty) next else getAncestorsRecursive(parent.get, next)
    }
  }
}

abstract class Tree[L <: Leaf[L] : ClassTag ] (val root:L) {

  def getDepth(depth: Int, nodes:Iterable[L] = List(root)): Array[L] = {
    if (depth <= 0) nodes.toArray else getDepth( depth-1, nodes.flatMap{ _.getChildren() })
  }

  def collectLeaves(layer:Iterable[L] = List(root), collected:Iterable[L]=ListBuffer() ):Array[L] = {
    if (layer.isEmpty) collected.toArray else {
      val nextLayer:Iterable[L] = layer.partition(_.getChildren().isEmpty)._2
      collectLeaves(nextLayer.flatMap(n => n.getChildren()), collected ++ layer)
    }
  }
}

abstract class Leaf[B <: Leaf[B]] (val observation:Char, val parent: Option[B] = None) extends Probablistic {
  def path():Iterable[Char] = Tree.getAncestorsRecursive(this.asInstanceOf[B]).map(_.observation).filterNot(_ == '\0')

  def getChildren():Iterable[B]

  def next(c:Char):Option[B]
}
