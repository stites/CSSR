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
  behavior of "loadHistory"

  before {
    AlphabetHolder.alphabet = Alphabet("abc".toArray)
  }

  it should "generate a single branch for an empty tree" in {
    val tree = Tree()
    Tree.loadHistory(tree, "abc")

    var children = tree.root.children
    assert(1 == children.size)
    var leaf = children.head
    assert("c".equals(leaf.observed))
    assert("c".head.equals(leaf.observation))

    children = children.head.children
    assert(1 == children.size)
    leaf = children.head
    assert("bc".equals(leaf.observed))
    assert("bc".head.equals(leaf.observation))

    children = children.head.children

    assert(1 == children.size)
    leaf = children.head
    assert("abc".equals(leaf.observed))
    assert("abc".head.equals(leaf.observation))
  }

  it should "generate multiple root branches if they do not exist" in {
    val tree = Tree()
    Tree.loadHistory(tree, "bc")
    Tree.loadHistory(tree, "aa")

    val children = tree.root.children
    assert(2 == children.size)

    val leaf1 = children(0)
    TreeTests.verifyLeafProperties(leaf1, "c")
    TreeTests.verifySingleChildrenProperties(leaf1.children, "bc")
    val leaf1Child = leaf1.children.head

    val leaf2 = children(1)
    TreeTests.verifyLeafProperties(leaf2, "a")
    TreeTests.verifySingleChildrenProperties(leaf2.children, "aa")
    val leaf2Child = leaf2.children.head

    Tree.loadHistory(tree, "ba")

    // Test final sizes
    assert(2 == children.size)
    assert(1 == leaf1.children.size)
    assert(2 == leaf2.children.size)
    assert(0 == leaf2Child.children.size)
    assert(0 == leaf1Child.children.size)

    val leaf2NewChild = leaf2.children.filter(_ != leaf2Child).head
    TreeTests.verifyLeafProperties(leaf2NewChild, "ba")
  }
}
