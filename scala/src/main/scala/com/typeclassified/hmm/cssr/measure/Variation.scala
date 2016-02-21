package com.typeclassified.hmm.cssr.measure

import breeze.linalg.sum
import com.typeclassified.hmm.cssr.measure.InferProbabilities.InferredDistribution
import com.typeclassified.hmm.cssr.trees.ParseLeaf

object Variation {

  def variation(dist:InferredDistribution, adjustedDataSize:Double): Double = {
    dist.foldLeft[Double] (0d) {
      case (total, (history:ParseLeaf, inferredProb:Double)) =>
        val historyProb:Double = sum(history.frequency / adjustedDataSize)

        total + math.abs(historyProb - inferredProb)
    }
  }

}
