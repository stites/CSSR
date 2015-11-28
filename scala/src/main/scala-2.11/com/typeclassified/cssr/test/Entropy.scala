package com.typeclassified.cssr.test

import breeze.linalg.{sum, DenseVector}
import breeze.numerics.log
import com.typeclassified.cssr.parse.{ParseAlphabet, ParseTree}

object Entropy {

  def calculateRelativeEntropy (parseTree:ParseTree, parseAlphabet: ParseAlphabet): Unit = {
    // We can't begin a substring of length maxLength at the last (maxLength-1)
    // positions in the data string
    val gArray = parseTree.getDepth(parseTree.maxLength)
    val stringProbs = DenseVector.zeros(gArray.length)

    CalcStringProbs(&g_array, parseTree.maxLength, hashtable, stringProbs);

    var relEntropy:Double = 0
    var logRatio:Double = _

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
