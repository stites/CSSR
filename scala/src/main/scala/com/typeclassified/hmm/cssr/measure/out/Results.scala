package com.typeclassified.hmm.cssr.measure.out

import breeze.linalg.{sum, VectorBuilder, DenseVector}
import _root_.com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}
import com.typeclassified.hmm.cssr.state.{Machine, EquivalenceClass}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object Results {
  protected val logger = Logger(LoggerFactory.getLogger(Results.getClass))

  def logMetadata(config:Config, alphabet: Alphabet, allStates:ListBuffer[EquivalenceClass]): Unit = {
    val newMachine = new Machine(allStates)

    logger.info("")
    logger.info("Metadata")
    logger.info("=======================")
    config.toString.split("\n").foreach { logger.info(_) }

    logger.info("Results")
    logger.info("=======================")
    s"""Alphabet Size: ${alphabet.length}
       |Number of Inferred States: ${allStates.length}
       |Relative Entropy: ${"TBD"}
       |Relative Entropy Rate: ${"TBD"}
       |Statistical Complexity: ${"TBD"}
       |Entropy Rate: ${"TBD"}
       |Variation: ${"TBD"}
       |""".stripMargin.split("\n").foreach { logger.info(_) }
  }

  def logEquivalenceClassDetails(allStates:ListBuffer[EquivalenceClass]): Unit = {
    val newMachine = new Machine(allStates)
    logger.info("===FOUND EQUIVALENCE CLASSES ====")
    for ((eqClass, i) <- newMachine.states.view.zipWithIndex) {
      logger.info(s"equiv class $i:")
      logger.info(s"          P(state): ${newMachine.distribution(i)}")
      logger.info(s"  Probability Dist: ${eqClass.distribution.toString()}")
      logger.info(s"    Frequency Dist: ${eqClass.frequency.toString()}")
      eqClass.histories.toArray.sortBy(_.observed).foreach(h => logger.info(s"  $h"))
    }
  }

  def logTreeStats(tree:Tree, allStates:ListBuffer[EquivalenceClass]): Unit = {
    val machine = new Machine(allStates)

    def go (path:String): DenseVector[Double] = {
      val dist = VectorBuilder[Double](tree.alphabet.length, 0d)

      for (n <- 1 to path.length) {
        val mLeaf:Option[Leaf] = tree.navigateHistory(path.substring(0, path.length - n).toList)
        if (mLeaf.nonEmpty) {
          val leaf = mLeaf.get
          val i = tree.alphabet.map(leaf.observation)
          println(dist(i), leaf.frequency(i), leaf.observation, path, path.substring(0, path.length - n))
          dist.add(i, dist(i) + leaf.frequency(i))
        }
      }
      return dist.toDenseVector
    }

    val statePartialTransitionFreq:Array[Array[DenseVector[Double]]] = machine.statePaths.map(_.map(go))

    val stateTransitionFreq:Array[DenseVector[Double]] = statePartialTransitionFreq.map(_.reduceLeft((acc, dist) => acc :+ dist))

    logger.info("===TREE STATS====")

    val groupedLeafCounts:Map[EquivalenceClass, Map[Char, Int]] = tree.collectLeaves()
      .filterNot(_.observation == 0.toChar) // don't grab the null-observation
      .groupBy(_.currentEquivalenceClass)
      .mapValues(_
        .groupBy(_.observation)
        .mapValues(_.length))

    logger.info(s"equiv classes found: ${groupedLeafCounts.size}")

    for ((e, charCountMap) <- groupedLeafCounts) {
      logger.info(s"equiv class char counts for: ${e.getClass.getSimpleName}@${e.hashCode()}")
      for ((c, i) <- charCountMap) {
        logger.info(s"$c: $i")
      }
    }

    for ((s, i) <- stateTransitionFreq.view.zipWithIndex) {
      logger.info(s"equiv class freq counts for: $i")
      println(s)
      println(s:/sum(s))
    }
  }
}
