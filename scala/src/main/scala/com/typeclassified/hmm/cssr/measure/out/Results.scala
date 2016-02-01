package com.typeclassified.hmm.cssr.measure.out

import breeze.linalg.{sum, VectorBuilder, DenseVector}
import _root_.com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}
import com.typeclassified.hmm.cssr.state.{AllStates, Machine, EquivalenceClass}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

/**
  * todo: replace strings with output stream
  *       add "change output" configuration.
  */
object Results {
  protected val logger = Logger(LoggerFactory.getLogger(Results.getClass))

  def measurements(alphabet: Alphabet, tree:Tree, machine: Machine, allStates: AllStates): String = {
    s"""Results
       |=======================
       |Alphabet Size: ${alphabet.length}
       |Data Size: ${tree.adjustedDataSize}
       |Relative Entropy: ${machine.relativeEntropy}
       |Relative Entropy Rate: ${machine.relativeEntropyRate}
       |Statistical Complexity: ${machine.statisticalComplexity}
       |Entropy Rate: ${machine.entropyRate}
       |Variation: ${machine.variation}
       |Number of Inferred States: ${allStates.states.length}
       |""".stripMargin
  }

  def metadata(config: Config):String = "Metadata\n=======================\n" + config.toString

  def dotInfo (config: Config, alphabet: Alphabet, allStates:AllStates): String = {
    val info = s"""digraph ${config.dataFile.getCanonicalPath} {
      |size = \"6,8.5\";
      |ratio = \"fill\";
      |node [shape = circle];
      |node [fontsize = 24];
      |edge [fontsize = 24];
      |""".stripMargin

    allStates.states
      .zipWithIndex
      .map {
        case (state, i) =>
          state.distribution
            .toArray
            .view.zipWithIndex
            .foldLeft[String]("") {
              case (memo, (prob, k)) if prob <= 0 => memo
              case (memo, (prob, k)) => memo + s"""$i -> $k [label = "${alphabet.raw(k)}: ${"%.7f".format(prob)}"];\n"""
            }
      }
      .reduceLeft(_+_)
  }

  def stateDetails(allStates: AllStates, alphabet: Alphabet): String = {
    allStates.states
      .view
      .zipWithIndex
      .map {
        case (eqClass, i) =>
          s"""State $i:
              |        P(state): ${allStates.distribution(i)}
              |        Alphabet: ${alphabet.toString}
              |Probability Dist: ${eqClass.distribution.toString()}
              |  Frequency Dist: ${eqClass.frequency.toString()}
              |     transitions: ${allStates.transitions(i)}
              |""".stripMargin +
            eqClass.histories.toArray.sortBy(_.observed).map{_.toString}.mkString("\n")
      }
      .mkString("\n")
  }
}
