package com.typeclassified.hmm.cssr.measure.out

import com.typeclassified.hmm.cssr.EquivalenceClass
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object Results {
  protected val logger = Logger(LoggerFactory.getLogger(Results.getClass))

  def logEquivalenceClasses(allStates:ListBuffer[EquivalenceClass]): Unit = {
    logger.info("===FOUND EQUIVALENCE CLASSES ====")
    logger.info("")
    for ((eqClass, idx) <- allStates.zip(Stream from 1)) {
      logger.info(s"equiv class $idx:")
      eqClass.histories.foreach(h => println(s"  ${h.observed}"))
    }
    logger.info("")
    logger.info("=================================")
    logger.info("")
  }
}
