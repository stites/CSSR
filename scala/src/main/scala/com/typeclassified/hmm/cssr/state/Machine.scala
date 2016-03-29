package com.typeclassified.hmm.cssr.state

import com.typeclassified.hmm.cssr.measure._
import com.typeclassified.hmm.cssr.measure.{InferProbabilities => I}
import com.typeclassified.hmm.cssr.trees.ParseTree
import com.typesafe.scalalogging.LazyLogging

class Machine (allStates: AllStates, tree:ParseTree) extends LazyLogging {
  val inferredDistribution:I.InferredDistribution = I.inferredDistribution(tree, tree.maxLength, allStates)

  val variation:Double             = Variation.variation(inferredDistribution, tree.adjustedDataSize)
  val entropyRate:Double           = EntropyRate.entropyRate(allStates)
  val relativeEntropy:Double       = RelativeEntropy.relativeEntropy(inferredDistribution, tree.adjustedDataSize) // <===
  val relativeEntropyRate:Double   = RelativeEntropyRate.relativeEntropyRate(inferredDistribution, tree, allStates)
  val statisticalComplexity:Double = StatisticalComplexity.cMu(allStates.distribution)
}

