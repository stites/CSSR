package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.{Probablistic, EquivalenceClass}
import org.scalatest.{FlatSpec, Matchers, BeforeAndAfter}

class LeafTests extends FlatSpec with Matchers with BeforeAndAfter {
  var tree:Tree = null

  before {
    AlphabetHolder.alphabet = Alphabet("abc".toCharArray)
    tree = Tree()
  }

  behavior of "updateDistribution"

  it should "update distributions for observing the _next_ values in history" in {
    val leaf = Leaf("a", tree, EquivalenceClass())
    leaf.updateDistribution('b')

    leaf.totalCounts          should be (1)
    leaf.frequency.toArray    should contain theSameElementsInOrderAs Array[Double](0, 1, 0)
    leaf.distribution.toArray should contain theSameElementsInOrderAs Array[Double](0, 1, 0)

    leaf.updateDistribution('b')
    leaf.updateDistribution('c')
    leaf.updateDistribution('a')
    leaf.totalCounts          should be (4)
    leaf.frequency.toArray    should contain theSameElementsInOrderAs Array[Double](   1,  2,   1)
    leaf.distribution.toArray should contain theSameElementsInOrderAs Array[Double](0.25,0.5,0.25)
  }

  behavior of "changeEquivalenceClass"

  it should "change the equivalence class for all observations with a longer history than itself" in {
    pending
  }

  behavior of "findChildWithAdditionalHistory"

  it should "find the next observation deeper into history with the given character" in {
    pending
  }

  behavior of "getStateOnTransitionTo"

  it should "return the state for a transition to the given observation" in {
    pending
  }
}
