package com.typeclassified.hmm.cssr.parse

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

import scala.collection.mutable.ListBuffer

object TreeTests {
  def verifySingleChildrenProperties(children:ListBuffer[Leaf], obs:String): ListBuffer[Leaf] = {
    assert(1 == children.size)
    verifyLeafProperties(children.head, obs)
    return children.head.children
  }

  def verifyLeafProperties(leaf:Leaf, obs:String): Unit = {
    assert(obs.equals(leaf.observed))
    assert(obs.head.equals(leaf.observation))
  }
}

class TreeTests extends FlatSpec with Matchers with BeforeAndAfter {
  var tree:Tree = null
  val alphabet = "abc"

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
    TreeTests.verifyLeafProperties(leaf1, "c")
    TreeTests.verifySingleChildrenProperties(leaf1.children, "bc")
    val leaf1Child = leaf1.children.head

    val leaf2 = children(1)
    TreeTests.verifyLeafProperties(leaf2, "a")
    TreeTests.verifySingleChildrenProperties(leaf2.children, "aa")
    val leaf2Child = leaf2.children.head

    Tree.loadHistory(tree, "ba")

    children should have size 2
    leaf1.children should have size 1
    leaf2.children should have size 2
    leaf2Child.children should have size 0
    leaf1Child.children should have size 0

    val leaf2NewChild = leaf2.children.filter(_ != leaf2Child).head
    TreeTests.verifyLeafProperties(leaf2NewChild, "ba")
  }

  behavior of "loadData of the alphabet with maxL of 3"

  it should "load all complete moving windows: abc, ab, bc, a, b, c" in {
    tree = Tree.loadData(alphabet.toArray, 3)
    var children:ListBuffer[Leaf] = null
    var expected = Seq("a", "b", "c")

    children = tree.root.children
    children should have size 3
    children.map(_.observed)    should contain theSameElementsAs expected
    children.map(_.observation) should contain theSameElementsAs expected.map(_.head)

    children = children.flatMap(_.children)
    expected = Seq("ab", "bc")
    children.map(_.observed)    should contain theSameElementsAs expected
    children.map(_.observation) should contain theSameElementsAs expected.map(_.head)

    children = children.flatMap(_.children)
    expected = Seq("abc")
    children.map(_.observed)    should contain theSameElementsAs expected
    children.map(_.observation) should contain theSameElementsAs expected.map(_.head)

    children = children.flatMap(_.children)
    children should have size 0
  }

  behavior of "navigateHistory"

  it should "be able to return the correct leaf" in {
    pending
  }
}
