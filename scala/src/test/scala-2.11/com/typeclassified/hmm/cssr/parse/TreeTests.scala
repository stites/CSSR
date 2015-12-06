package com.typeclassified.hmm.cssr.parse

import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, FlatSpec}

import scala.collection.mutable.ListBuffer

class TreeTests extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    AlphabetHolder.alphabet = Alphabet("abc".toArray)
  }

  behavior of "loadHistory"

  it should "generate a single branch for an empty tree" in {
    def verify (children:ListBuffer[Leaf], obs:String): ListBuffer[Leaf] = {
      assert(1 == children.size)
      assert(obs.equals(children.head.observed))
      assert(obs.head.equals(children.head.observation))
      return children.head.children
    }

    val tree = Tree()
    val obs = "abc".toArray
    Tree.loadHistory(tree, obs)

    val olderHistory = verify(tree.root.children, "c")
    val oldestHistory = verify(olderHistory, "bc")
    verify(oldestHistory, "abc")
  }
}
