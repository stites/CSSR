package com.typeclassified.hmm.cssr.measure

import com.typeclassified.hmm.cssr.measure.InferProbabilities.InferredDistribution
import com.typeclassified.hmm.cssr.state.AllStates
import com.typeclassified.hmm.cssr.trees.{ParseLeaf, ParseTree}
import com.typesafe.scalalogging.LazyLogging

object RelativeEntropyRate extends MathUtils with LazyLogging {
  def relativeEntropyRate(maxLengthDist:InferredDistribution, tree: ParseTree, allStates: AllStates):Double = {
    logger.debug("Relative Entropy Rate")
    logger.debug("===========================")

    val nextLastHistoryDist = InferProbabilities.inferredDistribution(tree, tree.maxLength-1, allStates)

    val relativeEntropyRate:Double = nextLastHistoryDist.foldLeft(0d) {
      case (partialRelEntRate, (leaf, inferredProb)) =>
        val totalRelEntRate = partialRelEntRate + relEntropyRateForHistory(leaf, inferredProb, tree, allStates)
//        logger.debug(s"totalRelEntRate: $totalRelEntRate")
        totalRelEntRate
    }

    relativeEntropyRate
  }

  // the frequency of occurrence of the history with that particular alpha symbol
  protected def relEntropyRateForHistory(history: ParseLeaf, inferredProb:Double, tree: ParseTree, allStates: AllStates):Double = {
    logger.debug(s"stringProb: $inferredProb, for history: ${history.toString}")

    val relEntRateHistTotal:Double = tree.alphabet
      .raw
      .foldLeft(0d){
        (relEntRateHist, alpha) => {
          val histFreqByAlpha = history.frequency(tree.alphabet.map(alpha)) / history.totalCounts
          val (inferredRatio, relEntRateAlpha) = relEntropyRateByNextAlphabet(history.observed, inferredProb, tree, allStates, histFreqByAlpha, alpha)

          logger.debug(s"relEntRateAlpha: $relEntRateAlpha")
          logger.debug(s"relEntRateHist: ${relEntRateHist + relEntRateAlpha}")

          relEntRateHist + relEntRateAlpha
        }
      }

    val histProbability:Double = history.totalCounts / tree.adjustedDataSize

    logger.debug(s"histFrequency: $histProbability")

    if (relEntRateHistTotal < 0) 0 else relEntRateHistTotal * histProbability
  }

  protected def relEntropyRateByNextAlphabet(history:String, inferredProb:Double, tree:ParseTree, allStates: AllStates, histFreqByAlpha:Double, alpha:Char)
  :(Double, Double) = {
    val isValid:Boolean = histFreqByAlpha > 0 && inferredProb > 0
    val isADisaster:Boolean = histFreqByAlpha > 0 && inferredProb <= 0

    if (isADisaster) {
      // TODO: fill this out formally, later
      logger.error("Something disastrous just happened")
    }

    val childStringProb = InferProbabilities.inferredHistory(alpha + history, tree, allStates)
    // eliminate branching? depends on scala's ln behavior as well as how it treats infinities
    val inferredRatio:Double = if (isValid) childStringProb / inferredProb else 0
    val relEntRateAlpha:Double = if (isValid) discreteEntropy(histFreqByAlpha, inferredRatio) else 0

    logger.debug(s"string: $history, plus alpha: $alpha")
    logger.debug(s"childStringProb: $childStringProb")
    logger.debug(s"stringProb: $inferredProb")
    logger.debug(s"inferredRatio: $inferredRatio")
    logger.debug(s"dataDist: $histFreqByAlpha")
    logger.debug(s"logRatio: $inferredRatio")

    (inferredRatio, relEntRateAlpha)
  }

}
