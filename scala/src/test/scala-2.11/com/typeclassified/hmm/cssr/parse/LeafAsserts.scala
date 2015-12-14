package com.typeclassified.hmm.cssr.parse

import org.scalatest.Matchers

import scala.collection.mutable.ListBuffer

trait LeafAsserts extends Matchers {
  def assertLeafProperties(leaf:Leaf, obs:String): Unit = {
    leaf.observed should equal (obs.reverse)
    leaf.observation should equal (obs.head)
  }

  def assertChildrenByExactBatch(children:ListBuffer[Leaf], expected:Seq[String]) = {
    children should have size expected.length
    children.map(_.observed)    should contain theSameElementsAs expected.map(_.reverse)
    children.map(_.observation) should contain theSameElementsAs expected.map(_.head)
  }
}
