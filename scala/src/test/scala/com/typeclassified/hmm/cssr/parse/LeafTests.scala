package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.shared.ProbablisticAsserts
import com.typeclassified.hmm.cssr.state.EquivalenceClass
import org.scalatest.{FlatSpec, Matchers, BeforeAndAfter}

import scala.collection.mutable.ListBuffer

class LeafTests extends FlatSpec with Matchers with ProbablisticAsserts with LeafAsserts with BeforeAndAfter {
  var tree:Tree = null

  before {
    AlphabetHolder.alphabet = Alphabet("abc".toCharArray)
    tree = Tree(AlphabetHolder.alphabet)
  }

  "updateDistribution" should "update distributions for observing the _next_ values in history" in {
    val leaf = new Leaf("a", tree, EquivalenceClass())
    leaf.updateDistribution('b')
    assertProbabalisticDetails(leaf, Array(0,1,0))

    leaf.updateDistribution('b')
    assertProbabalisticDetails(leaf, Array(0,2,0))

    leaf.updateDistribution('c')
    assertProbabalisticDetails(leaf, Array(0,2,1))

    leaf.updateDistribution('a')
    assertProbabalisticDetails(leaf, Array(1,2,1))
  }

  behavior of "addChild"

  it should "give the correct observations" in {
    var leaf = tree.root
    leaf.addChild('b')
    leaf = leaf.children.head
    assertLeafProperties(leaf, "b")

    leaf.addChild('a')
    leaf = leaf.children.head
    assertLeafProperties(leaf, "ab")

    leaf.addChild('c')
    leaf = leaf.children.head
    assertLeafProperties(leaf, "cab")
  }

  it should "run updateDistribution while adding a child" in {
    val leaf = tree.root
    leaf.addChild('b')
    assertProbabalisticDetails(leaf, Array(0,1,0))
    leaf.children should have size 1

    leaf.addChild('a')
    assertProbabalisticDetails(leaf, Array(1,1,0))
    leaf.children should have size 2

    leaf.children.head.addChild('a')
    assertProbabalisticDetails(leaf.children.head, Array(1,0,0))
    leaf.children.head.children should have size 1
  }

  it should "not introduce children with the same observed value" in {
    val leaf = tree.root
    leaf.addChild('b')
    assertProbabalisticDetails(leaf, Array(0,1,0))
    leaf.children should have size 1

    leaf.addChild('b')
    assertProbabalisticDetails(leaf, Array(0,2,0))
    leaf.children should have size 1

    leaf.addChild('a')
    assertProbabalisticDetails(leaf, Array(1,2,0))
    leaf.children should have size 2

    leaf.addChild('a')
    assertProbabalisticDetails(leaf, Array(2,2,0))
    leaf.children should have size 2
  }


  behavior of "changeEquivalenceClass"

  it should "change the equivalence class of a given leaf's branch" in {
    Tree.loadHistory(tree, "abc".toCharArray)
    Tree.loadHistory(tree, "bca".toCharArray)
    val originalClass = tree.root.currentEquivalenceClass
    val originalClassLeaves = tree.collectLeaves()
    for (leaf <- originalClassLeaves) {
      leaf.currentEquivalenceClass should equal(originalClass)
    }
    val newEC_A = EquivalenceClass()
    val newEC_C = EquivalenceClass()
    val rootLeafA = tree.root.children.find(_.observation == 'a').get
    val rootLeafC = tree.root.children.find(_.observation == 'c').get
    rootLeafA.changeEquivalenceClass(newEC_A)
    rootLeafC.changeEquivalenceClass(newEC_C)
    val branchA = tree.collectLeaves(ListBuffer(rootLeafA))
    val branchC = tree.collectLeaves(ListBuffer(rootLeafC))

    tree.root.currentEquivalenceClass should equal(originalClass)
    for (leaf <- branchA) {
      leaf.currentEquivalenceClass should equal(newEC_A)
    }
    for (leaf <- branchC) {
      leaf.currentEquivalenceClass should equal(newEC_C)
    }
  }
}
