package com.typeclassified.hmm.cssr.parse

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

import scala.collection.mutable.ListBuffer

class TreeTests extends FlatSpec with Matchers with BeforeAndAfter {
  var tree:Tree = null
  val alphabet = "abc"

  def assertLeafProperties(leaf:Leaf, obs:String): Unit = {
    leaf.observed should equal (obs)
    leaf.observation should equal (obs.head)
  }

  before {
    AlphabetHolder.alphabet = Alphabet(alphabet.toArray)
    tree = Tree()
  }

  behavior of "loadHistory"

  it should "generate a single branch for an empty tree" in {
    var children:ListBuffer[Leaf] = null
    var leaf:Leaf = null
    Tree.loadHistory(tree, "abc")

    children = tree.root.children
    children should have size 1

    leaf = children.head
    leaf.observed should equal ("c")
    leaf.observation should equal ("c".head)

    children = children.head.children
    children should have size 1

    leaf = children.head
    leaf.observed should equal ("bc")
    leaf.observation should equal ("bc".head)

    children = children.head.children
    children should have size 1

    leaf = children.head
    leaf.observed should equal ("abc")
    leaf.observation should equal ("abc".head)
  }

  it should "generate multiple root branches if they do not exist" in {
    Tree.loadHistory(tree, "bc")
    Tree.loadHistory(tree, "aa")

    val children = tree.root.children
    children should have size 2

    val leaf1 = children(0)
    assertLeafProperties(leaf1, "c")
    leaf1.children should have size 1
    val leaf1Child = leaf1.children.head
    assertLeafProperties(leaf1Child, "bc")


    val leaf2 = children(1)
    assertLeafProperties(leaf2, "a")
    leaf2.children should have size 1
    val leaf2Child = leaf2.children.head
    assertLeafProperties(leaf2Child, "aa")


    Tree.loadHistory(tree, "ba")

    children should have size 2
    leaf1.children should have size 1
    leaf2.children should have size 2
    leaf2Child.children should have size 0
    leaf1Child.children should have size 0

    val leaf2NewChild = leaf2.children.filter(_ != leaf2Child).head
    assertLeafProperties(leaf2NewChild, "ba")
  }

  behavior of "loadData"

  it should "load all complete moving windows for 'abc' (lMax==3): abc, ab, bc, a, b, c" in {
    tree = Tree.loadData(alphabet.toArray, 3)

    var children:ListBuffer[Leaf] = tree.root.children
    assertChildrenByExactBatch(children, Seq("a", "b", "c"))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, Seq("ab", "bc"))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, Seq("abc"))

    children = children.flatMap(_.children)
    children should have size 0
  }

  it should "load all combinations of our alphabet when givin 'abcabc' (lMax==3): abc, bca, cab, ab, bc, ca, a, b, c" in {
    tree = Tree.loadData("abcabc".toArray, 3)

    var children:ListBuffer[Leaf] = tree.root.children
    assertChildrenByExactBatch(children, Seq("a", "b", "c"))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, Seq("ab", "bc", "ca"))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, Seq("abc", "bca", "cab"))

    children = children.flatMap(_.children)
    children should have size 0
  }


  it should """give root-b three children, given 'abcbabcbb' (@lMax==3):
      | abc, bcb, cba, bab, cbb, ab, bc, cb, ba, bb, a, b, c """.stripMargin in {
    tree = Tree.loadData("abcbabcbb".toArray, 3)

    var children:ListBuffer[Leaf] = tree.root.children
    assertChildrenByExactBatch(children, Seq("a", "b", "c"))

    assertChildrenByExactBatch(children.filter(_.observation == 'b').head.children, Seq("ab", "cb", "bb"))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, Seq("ab", "bc", "cb", "ba", "bb"))

    assertChildrenByExactBatch(children.filter(_.observation == 'b').flatMap(_.children), Seq("abc", "cba", "cbb"))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, Seq("abc", "bcb", "cba", "bab", "cbb"))

    children = children.flatMap(_.children)
    children should have size 0
  }

  def assertChildrenByExactBatch(children:ListBuffer[Leaf], expected:Seq[String]) = {
    children should have size expected.length
    children.map(_.observed)    should contain theSameElementsAs expected
    children.map(_.observation) should contain theSameElementsAs expected.map(_.head)
  }

  behavior of "navigateHistory"

  it should "be able to return the correct leaf" in {
    pending
  }
}
