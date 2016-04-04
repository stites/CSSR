package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.trees.ParseLeaf
import org.scalatest.Matchers

import scala.collection.mutable.ListBuffer

trait LeafAsserts extends Matchers {
  def assertLeafProperties(leaf:ParseLeaf, obs:String): Unit = {
    leaf.observed    should equal (obs)
    leaf.observation should equal (obs.head)
  }

  def assertChildrenByExactBatch(pleaf:ParseLeaf, expected:Seq[String]):Unit = assertChildrenByExactBatch(pleaf.children, expected)

  def assertChildrenByExactBatch(children:ListBuffer[ParseLeaf], expected:Seq[String]):Unit = {
    children                    should have    size              expected.length
    children.map(_.observed)    should contain theSameElementsAs expected
    children.map(_.observation) should contain theSameElementsAs expected.map(_.head)
  }
}
