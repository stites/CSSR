package com.typeclassified.hmm.cssr.measure.out

import breeze.linalg.{sum, VectorBuilder, DenseVector}
import com.typeclassified.hmm.cssr.parse.{Leaf, Tree}
import com.typeclassified.hmm.cssr.state.{Machine, EquivalenceClass}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Results {
  protected val logger = Logger(LoggerFactory.getLogger(Results.getClass))

  def logEquivalenceClasses(allStates:ListBuffer[EquivalenceClass]): Unit = {
    val newMachine = new Machine(allStates)

    logger.info("===FOUND EQUIVALENCE CLASSES ====")

    for ((eqClass, i) <- newMachine.states.view.zipWithIndex) {
      logger.info(s"equiv class $i:")
      logger.info(s"  Probability: ${newMachine.distribution(i)}")
      logger.info(s"    Frequency: ${newMachine.frequency(i)}")
      eqClass.histories.foreach(h => println(s"  ${h.observed}"))
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
      logger.info(s"equiv class char counts for: $e")
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
