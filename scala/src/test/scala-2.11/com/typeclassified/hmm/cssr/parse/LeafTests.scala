package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.{Probablistic, EquivalenceClass}
import org.scalatest.{FlatSpec, Matchers, BeforeAndAfter}

import scala.collection.mutable.ListBuffer

class LeafTests extends FlatSpec with Matchers with BeforeAndAfter {
  behavior of "updateDistribution"

  it should "update distributions for observing the _next_ values in history" in {
    pending

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
