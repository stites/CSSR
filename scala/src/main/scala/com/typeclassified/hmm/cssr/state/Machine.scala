package com.typeclassified.hmm.cssr.state

import com.typeclassified.hmm.cssr.measure._
import com.typeclassified.hmm.cssr.measure.{InferProbabilities => I}
import com.typeclassified.hmm.cssr.parse.Tree
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

object Machine {
  protected val logger = Logger(LoggerFactory.getLogger(Machine.getClass))
}

class Machine (allStates: AllStates, tree:Tree) {
  val inferredDistribution:I.InferredDistribution = I.inferredDistribution(tree, tree.maxLength, allStates)

  val variation:Double             = Variation.variation(inferredDistribution, tree.adjustedDataSize)
  val entropyRate:Double           = EntropyRate.entropyRate(allStates)
  val relativeEntropy:Double       = RelativeEntropy.relativeEntropy(inferredDistribution, tree.adjustedDataSize)
  val relativeEntropyRate:Double   = RelativeEntropyRate.relativeEntropyRate(inferredDistribution, tree, allStates)
  val statisticalComplexity:Double = StatisticalComplexity.cMu(allStates.distribution)
}

