package com.typeclassified.hmm.cssr.measure

import breeze.linalg.{DenseVector, sum}
import com.typeclassified.hmm.cssr.EquivalenceClass
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}

object RelativeEntropy {

  /**
    * calculates the probability of all the max length strings in the
    * data based on the inferred machine
    *
    * @param parseTree
    * @param S
    * @param alphabet
    */
  def calculateRelativeEntropy (parseTree:Tree, S:Array[Leaf], alphabet: Alphabet): Double = {
    // We can't begin a substring of length maxLength at the last (maxLength-1)
    // positions in the data string
    val gArray = parseTree.getDepth(parseTree.maxLength)
    val stringProbs = DenseVector.zeros[Double](gArray.length)

    HistoryProbablities.calcStringProbs(gArray, parseTree.maxLength, S, alphabet, stringProbs)

    var relEntropy:Double = 0
    var logRatio:Double = 0

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

    return if (relEntropy > 0) relEntropy else 0
  }


}
