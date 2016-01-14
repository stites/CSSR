package com.typeclassified.hmm.cssr.measure

import breeze.linalg.sum
import com.typeclassified.hmm.cssr.parse.Leaf
import com.typeclassified.hmm.cssr.state.Machine.InferredDistribution

object Variation {

  def variation(dist:InferredDistribution, adjustedDataSize:Double): Double = {
    dist.foldLeft[Double] (0d) { (total, pair) => {
      val (history:Leaf, inferredProbability:Double) = pair
      val historyProbability:Double = sum(history.frequency / adjustedDataSize)
      total + math.abs(historyProbability - inferredProbability)
    } }
  }

}
