package com.typeclassified.hmm.cssr.measure

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.log
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}
import com.typeclassified.hmm.cssr.state.{Machine, EquivalenceClass}

import scala.collection.mutable.ArrayBuffer

object RelativeEntropy {
  /**
    * calculates the probability of all the max length strings in the
    * data based on the inferred machine:
    *
    * Kullback-Leibler Distance:
    *
    * \begin{equation}
    *    d= \sum_{k} p_k * \log_2 * { p_k \over q_k }
    * \end{equation}
    *
    * @param parseTree
    * @param machine
    * @param alphabet
    */
  def kullbackLeiblerDistance (parseTree:Tree, machine:Machine, alphabet: Alphabet): Double = {
    // these are our Ps
    val ps:Array[Array[Leaf]] = machine.states.map(_.histories.toArray)
    val pStateProbDist:DenseVector[Double] = getProbabilityDistribution(ps, parseTree)

    // these are our Qs
    val observedEqClasses:Map[EquivalenceClass, Array[Leaf]] = parseTree.collectLeaves(parseTree.root.children).groupBy(_.currentEquivalenceClass)
    val qs:Array[Array[Leaf]] = machine.states.map {
      eq => observedEqClasses(eq).filter(_.observed.length == parseTree.maxLength)
    }
    val qStateProbDist:DenseVector[Double] = getProbabilityDistribution(qs, parseTree)

    sum( pStateProbDist :* (log(pStateProbDist) :/ (log(qStateProbDist) :* math.log(2))) )
  }

  def getLeafFreqDistributions(leavesByState:Array[Array[Leaf]], tree: Tree): Array[DenseVector[Double]] = {
    leavesByState.map { s =>
      new DenseVector( s.map { h =>
        tree.navigateHistory(h.observed.init.toList) match {
          case Some(leaf) => leaf.frequency(tree.alphabet.map(h.observation))
          case None => 0d
        } } )
    }
  }

  def getProbabilityDistribution(leavesByState:Array[Array[Leaf]], tree: Tree):DenseVector[Double] = {
    val frequencyDistributions:Array[DenseVector[Double]] = getLeafFreqDistributions(leavesByState, tree)
    val totalCount:Double = frequencyDistributions.foldLeft(0d)((acc, vec)=> acc + sum(vec))
    val probabilityDistributions:Array[DenseVector[Double]] = frequencyDistributions.map(_/totalCount)

    // ...or maybe these are what we want...
    val stateFreqDist:DenseVector[Double] = new DenseVector(frequencyDistributions.map(sum(_)))
    val stateProbDist:DenseVector[Double] = stateFreqDist / totalCount

    stateProbDist
  }






  // ============================================================================

  def original (parseTree:Tree, machine:Machine, alphabet: Alphabet): Double = {
//    HistoryProbablities.calcStringProbs(gArray, parseTree.maxLength, S, alphabet)

    var relEntropy:Double = 0
    var logRatio:Double = 0

    /*
    for ((history, i) <- gArray.view.zipWithIndex) {
      val occurrence:Double = sum(history.frequency)
      val dataProb:Double = occurrence / parseTree.adjustedDataSize

      if (dataProb > 0) {
        logRatio = math.log(dataProb) - math.log(stringProbs(i))
        logRatio *= dataProb
        relEntropy += logRatio
      }
    }
    relEntropy = relEntropy/math.log(2)
    */

    return if (relEntropy > 0) relEntropy else 0
  }
}
