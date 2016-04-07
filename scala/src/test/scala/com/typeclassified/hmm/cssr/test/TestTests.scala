package com.typeclassified.hmm.cssr.test

import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, Alphabet}
import com.typeclassified.hmm.cssr.state.EquivalenceClass
import com.typeclassified.hmm.cssr.trees.{ParseLeaf, ParseTree}
import org.scalatest.{FlatSpec, Matchers}

class TestTests extends FlatSpec with Matchers {
  AlphabetHolder.alphabet = new Alphabet("abc".toCharArray)
  var alphabet = AlphabetHolder.alphabet
  var tree = new ParseTree(alphabet, EquivalenceClass())

  behavior of "Test.move"

  it should "move a Leaf from one EquivalenceClass to another" in {
    val to   = EquivalenceClass()
    val from = tree.root.currentEquivalenceClass
    val leaf = new ParseLeaf("abc", from)

    leaf.currentEquivalenceClass should be (from)

    Test.move(leaf, from, null, to)

    leaf.currentEquivalenceClass should be (to)
  }
}
