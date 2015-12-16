package com.typeclassified.hmm.cssr.measure.out

import com.typeclassified.hmm.cssr.state.{Machine, EquivalenceClass}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object Results {
  protected val logger = Logger(LoggerFactory.getLogger(Results.getClass))

  def logEquivalenceClasses(allStates:ListBuffer[EquivalenceClass]): Unit = {
    val newMachine = new Machine(allStates)

    logger.info("===FOUND EQUIVALENCE CLASSES ====")
    logger.info("")

    for ((eqClass, i) <- newMachine.states.view.zipWithIndex) {
      logger.info(s"equiv class $i:")
      logger.info(s"  Probability: ${newMachine.distribution(i)}")
      logger.info(s"    Frequency: ${newMachine.frequency(i)}")
      eqClass.histories.foreach(h => println(s"  ${h.observed}"))
    }
    logger.info("")
    logger.info("=================================")
    logger.info("")
  }
}
