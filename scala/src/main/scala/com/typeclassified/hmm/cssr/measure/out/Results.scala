package com.typeclassified.hmm.cssr.measure.out

import breeze.linalg.{sum, VectorBuilder, DenseVector}
import _root_.com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}
import com.typeclassified.hmm.cssr.state.{Machine, EquivalenceClass}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

/**
  * todo: replace strings with output stream
  *       add "change output" configuration.
  */
object Results {
  protected val logger = Logger(LoggerFactory.getLogger(Results.getClass))

  def measurements(alphabet: Alphabet, machine: Machine): String = {
    s"""Results
       |=======================
       |Alphabet Size: ${alphabet.length}
       |Number of Inferred States: ${machine.states.length}
       |Relative Entropy: ${"TBD"}
       |Relative Entropy Rate: ${"TBD"}
       |Statistical Complexity: ${"TBD"}
       |Entropy Rate: ${"TBD"}
       |Variation: ${"TBD"}
       |""".stripMargin
  }

  def metadata(config: Config):String = "Metadata\n=======================\n" + config.toString

  def dotInfo (config: Config, alphabet: Alphabet, allStates:ListBuffer[EquivalenceClass]): String = {
    val info = s"""digraph ${config.dataFile.getCanonicalPath} {
      |size = \"6,8.5\";
      |ratio = \"fill\";
      |node [shape = circle];
      |node [fontsize = 24];
      |edge [fontsize = 24];
      |""".stripMargin

    val states = allStates.zipWithIndex.foldLeft("") {
      case (memo, (state, i)) => memo + state.distribution.toArray.zipWithIndex.foldLeft("") {
        case (memo, (prob, k)) => if (prob <= 0) memo else {
          memo + s"""$i -> ${k} [label = "${alphabet.raw(k)}: ${"%.7f".format(prob)}"];\n"""
        }
      }
    }
    info + states + "}\n"
  }

  def stateDetails(machine: Machine): String = {
    machine.states.view.zipWithIndex.map {
      case (eqClass, i) =>
        s"""State $i:
        |        P(state): ${machine.distribution(i)}
        |Probability Dist: ${eqClass.distribution.toString()}
        |  Frequency Dist: ${eqClass.frequency.toString()}
        |""".stripMargin +
        eqClass.histories.toArray.sortBy(_.observed).map{_.toString}.mkString("\n")
    }.mkString("\n")
  }

  @Deprecated
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
