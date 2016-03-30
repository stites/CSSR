package com.typeclassified.hmm.cssr.measure

import com.typeclassified.hmm.cssr.measure.InferProbabilities.InferredDistribution
import com.typeclassified.hmm.cssr.shared.Level.Level
import com.typeclassified.hmm.cssr.shared.{Level, Logging}

object RelativeEntropy extends MathUtils with Logging {
  override def loglevel() = Level.OFF
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
    * @param dist the inferred distribution of the *histories of max length*.
    * @param adjustedDataSize
    * @return
    */
  def relativeEntropy(dist:InferredDistribution, adjustedDataSize:Double):Double = {
    debug("Relative Entropy")
    debug("===========================")

    // FIXME : this _should not be <0_ however calculations provide the contrary
    // when the generated, inferred probability is greater than the observed one - we find the added log-ratio is < 0
    // currently, we have too many of that for the even process. This also begs the question: should there _ever_ be
    // a calculation where the log-ratio is < 0, since it was permissible in the C++. It may be that this is not the case
    // since I believe we are using K-L distribution for conditional, empirical distributions (yes?)
    val relativeEntropy:Double = dist.foldLeft(0d) {
      case (incrementalRelEnt, (leaf, inferredProb)) =>
        val observedProb = leaf.totalCounts / adjustedDataSize
        debug(s"${leaf.toString}")
        debug(s"historyProb: $observedProb")

        // it seems to me that we should be checking if the inferred probability is > 0.
        // By virtue of this: should the conditional be flipped? Note: this makes the final rel entropy non-negative
        //        if (inferredProb > 0){
        //          val logRatio = math.log(inferredProb / observedProb) // note that math.log in scala is the natural log
        //          val cacheRE = incrementalRelEnt + inferredProb * logRatio
        if (observedProb > 0){
          val cacheRE = incrementalRelEnt + discreteEntropy(observedProb, inferredProb)
          debug(s"inferredProb: $inferredProb")
          debug(s"logRatio:${math.log(observedProb / inferredProb)}")
          debug(s"incrementalRelEnt:$cacheRE")
          cacheRE
        } else {
          //          logger.debug(s"NO AGGREGATION! dataProb: $inferredProb")
          debug(s"NO AGGREGATION! dataProb: $observedProb")
          incrementalRelEnt
        }
    }
    if (relativeEntropy < 0) 0 else relativeEntropy
  }

}
