package com.typeclassified.hmm.cssr.trees

import breeze.linalg.DenseVector
import breeze.numerics._
import com.typeclassified.hmm.cssr.shared.{Epsilon, Probablistic}

import scala.collection.mutable.ListBuffer

object Tree {
  implicit val ep:Epsilon = new Epsilon(0.01)

  def matches[L1 <: Leaf, L2 <: Leaf](u:L1)(w:L2): Boolean = u ~= w

  def matches(u:DenseVector[Double])(w:DenseVector[Double]): Boolean = round(u) == round(w)

  def round(dist:DenseVector[Double]): DenseVector[Double] = {
    val rndPrecision:Double = 1 / ep.precision
    (rint(dist * rndPrecision):DenseVector[Double]) / rndPrecision
  }

  def round[L1 <: Leaf](u:L1): DenseVector[Double] = round(u.distribution)

  def firstExcisable[L <: Leaf](w:L):Option[L] = {
    val ancestors = ListBuffer[L]()
    var active = w
    while (active.parent.nonEmpty) {
      ancestors += active.parent.get.asInstanceOf[L]
      active = active.parent.get.asInstanceOf[L]
    }
    // ancestors must be ordered by depth with the root first. Hence, reverse
    ancestors.reverse.find{matches(w)}
  }
}
class Tree {

}

class Leaf (val parent: Option[Leaf] = None) extends Probablistic {
  implicit val ep:Epsilon = new Epsilon(0.01)

  def ~=(pLeaf:Leaf)(implicit ep:Epsilon):Boolean = {
    (abs(distribution :- pLeaf.distribution) :< ep.precision).reduceRight(_&&_)
  }
}
