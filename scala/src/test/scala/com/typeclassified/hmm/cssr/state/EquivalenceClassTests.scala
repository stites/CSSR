package com.typeclassified.hmm.cssr.state

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.hmm.cssr.parse.{Alphabet, AlphabetHolder}
import com.typeclassified.hmm.cssr.shared.ProbablisticAsserts
import com.typeclassified.hmm.cssr.trees.{ParseLeaf, ParseTree}
import org.apache.commons.math3.stat.Frequency
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

/**
  * it makes a lot more sense to split out the updateDistribution tests, but for expediency (avoiding scalamocks, don't
  * ask why) we're sticking to this.
  */
class EquivalenceClassTests extends FlatSpec with Matchers with ProbablisticAsserts with BeforeAndAfter {
  var tree:ParseTree = null

  before {
    AlphabetHolder.alphabet = Alphabet("abc".toCharArray)
    tree = ParseTree(AlphabetHolder.alphabet)
  }

  behavior of "addHistory"

  it should "add a history to the equivalence class and normalize the histories" in {
    val eq = new State()
    val leaf = new ParseLeaf("abc")
    val leaf2 = new ParseLeaf("cbc")
    val (frequency, totalCounts) = (new DenseVector[Double](Array(1d,2d,3d)), 6d)
    for (l <- List(leaf, leaf2)){
      l.frequency = frequency
      l.totalCounts = totalCounts
    }

    eq.addHistory(leaf)
    assertEquivalenceClassProperties(eq, 1, frequency, totalCounts, frequency/totalCounts)

    eq.addHistory(leaf)
    assertEquivalenceClassProperties(eq, 1, frequency, totalCounts, frequency/totalCounts)

    eq.addHistory(leaf2)
    assertEquivalenceClassProperties(eq, 2, frequency.map(_*2), totalCounts*2)
  }

  behavior of "rmHistory"

  it should "remove a history to the equivalence class and normalize the histories" in {
    val eq = new State()
    val zeros = DenseVector.zeros[Double](3)
    val leaf = new ParseLeaf("abc")
    val leaf2 = new ParseLeaf("cbc")
    val (frequency, totalCounts) = (new DenseVector[Double](Array(1d,2d,3d)), 6d)
    for (l <- List(leaf, leaf2)){
      l.frequency = frequency
      l.totalCounts = totalCounts
    }

    eq.addHistory(leaf)
    eq.addHistory(leaf2)

    eq.rmHistory(leaf)
    assertEquivalenceClassProperties(eq, 1, frequency, totalCounts, frequency/totalCounts)

    eq.rmHistory(leaf2)
    assertEquivalenceClassProperties(eq, 0, zeros, 0, zeros)

    eq.rmHistory(leaf2)
    assertEquivalenceClassProperties(eq, 0, zeros, 0, zeros)
  }

  def assertEquivalenceClassProperties(eq:State,
                                       numHistories:Int,
                                       frequency: DenseVector[Double],
                                       totalCounts:Double)
  :Unit = {
    eq.histories should have size numHistories
    eq.frequency.toArray should contain theSameElementsInOrderAs frequency.toArray
    eq.totalCounts should be (totalCounts)
  }

  def assertEquivalenceClassProperties(eq:State,
                                       numHistories:Int,
                                       frequency: DenseVector[Double],
                                       totalCounts:Double,
                                       distribution: DenseVector[Double])
  :Unit = {
    assertEquivalenceClassProperties(eq, numHistories, frequency, totalCounts)
    eq.distribution.toArray should contain theSameElementsInOrderAs distribution.toArray
  }
}
