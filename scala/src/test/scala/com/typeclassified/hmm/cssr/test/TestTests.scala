package com.typeclassified.hmm.cssr.test

import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, Alphabet}
import com.typeclassified.hmm.cssr.state.State
import com.typeclassified.hmm.cssr.trees.{ParseLeaf, ParseTree}
import org.scalatest.{FlatSpec, Matchers}

class TestTests extends FlatSpec with Matchers {
  AlphabetHolder.alphabet = new Alphabet("abc".toCharArray)
  var alphabet = AlphabetHolder.alphabet
  var tree = new ParseTree(alphabet)

  behavior of "Test.move"

  it should "move a Leaf from one EquivalenceClass to another" in {
    pending
  }
}
