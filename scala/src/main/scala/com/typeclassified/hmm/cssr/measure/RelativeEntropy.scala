package com.typeclassified.hmm.cssr.measure

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.log
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}
import com.typeclassified.hmm.cssr.state.Machine.InferredDistribution
import com.typeclassified.hmm.cssr.state.{Machine, EquivalenceClass}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

object RelativeEntropy {
  protected val logger = Logger(LoggerFactory.getLogger(RelativeEntropy.getClass))

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
    * @param dist
    * @param adjustedDataSize
    * @return
    */
  def kullbackLeiblerDistance(dist:InferredDistribution, adjustedDataSize:Double):Double = {
    logger.debug("Relative Entropy")
    logger.debug("===========================")

    // FIXME : this _should not be <0_ however calculations provide the contrary
    // when the generated, inferred probability is greater than the observed one - we find the added log-ratio is < 0
    // currently, we have too many of that for the even process. This also begs the question: should there _ever_ be
    // a calculation where the log-ratio is < 0, since it was permissible in the C++. It may be that this is not the case
    // since I believe we are using K-L distribution for conditional, empirical distributions (yes?)
    val relativeEntropy:Double = dist.foldLeft(0d) {
      case (incrementalRelEnt, (leaf, inferredProb)) =>
        val observedFrequency = leaf.totalCounts / adjustedDataSize
        logger.debug(s"${leaf.toString}")
        logger.debug(s"historyProb: $observedFrequency")

        // it seems to me that we should be checking if the inferred probability is > 0.
        // By virtue of this: should the conditional be flipped? Note: this makes the final rel entropy non-negative
        //        if (inferredProb > 0){
        //          val logRatio = math.log(inferredProb / observedFrequency) // note that math.log in scala is the natural log
        //          val cacheRE = incrementalRelEnt + inferredProb * logRatio
        if (observedFrequency > 0){
          val logRatio = math.log(observedFrequency / inferredProb) // note that math.log in scala is the natural log
          val cacheRE = incrementalRelEnt + observedFrequency * logRatio
          logger.debug(s"inferredProb: $inferredProb")
          logger.debug(s"logRatio:$logRatio")
          logger.debug(s"incrementalRelEnt:$cacheRE")
          cacheRE
        } else {
          //          logger.debug(s"NO AGGREGATION! dataProb: $inferredProb")
          logger.debug(s"NO AGGREGATION! dataProb: $observedFrequency")
          incrementalRelEnt
        }
    }
    relativeEntropy
  }

}
