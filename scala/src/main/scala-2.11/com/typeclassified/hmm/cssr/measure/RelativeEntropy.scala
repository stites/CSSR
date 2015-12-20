package com.typeclassified.hmm.cssr.measure

import breeze.linalg.{DenseVector, sum}
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}
import com.typeclassified.hmm.cssr.state.{Machine, EquivalenceClass}

import scala.collection.mutable.ArrayBuffer

object RelativeEntropy {

  /**
    * calculates the probability of all the max length strings in the
    * data based on the inferred machine:
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
    // We can't begin a substring of length maxLength at the last (maxLength-1)
    // positions in the data string
    def getP(leaf: Option[Leaf], h:Leaf):Double = {
      return leaf.map(_.frequency(alphabet.map(h.observation))).getOrElse(0d)
    }

    // these are our Ps
    val pDistribution:Array[ArrayBuffer[Double]] = machine.states
      .map[EquivalenceClass, ArrayBuffer[Double]](s => {
      return ArrayBuffer[Double](0d)
      return s.histories.map[Leaf, Double](h => {
        val mLeaf: Option[Leaf] = parseTree.navigateHistory(h.observed.init.toList)
        return getP(mLeaf, h)
      })})



    // these are our Qs
    val nextIsMaxLengthStrings:Array[Leaf] = parseTree.getDepth(parseTree.maxLength - 1)
    maxLengthStrings.map[Leaf, Double](strElement => {
      return strElement.frequency
    })

    val stringProbs = DenseVector.zeros[Double](maxLengthStrings.length)

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
