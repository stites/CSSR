package com.typeclassified.hmm.cssr.measure.out

import java.io._

import _root_.com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.parse.Alphabet
import com.typeclassified.hmm.cssr.state.{AllStates, Machine}
import com.typeclassified.hmm.cssr.trees.ParseTree

class Results (config:Config, alphabet: Alphabet, tree:ParseTree, machine: Machine, allStates: AllStates) {
  val metadata:String = "Metadata\n=======================\n" + config.toString + "\n"

  protected val dotMeta:String = s"""digraph ${config.dataFile.getCanonicalPath} {
                              |size = \"6,8.5\";
                              |ratio = \"fill\";
                              |node [shape = circle];
                              |node [fontsize = 24];
                              |edge [fontsize = 24];
                              |""".stripMargin

  val dotInfo: String = dotMeta + allStates.states
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
    .reduceLeft(_+_) + "\n"

  val measurements: String = s"""Results
                                 |=======================
                                 |Alphabet Size: ${alphabet.length}
                                 |Data Size: ${tree.adjustedDataSize}
                                 |Relative Entropy: ${machine.relativeEntropy}
                                 |Relative Entropy Rate: ${machine.relativeEntropyRate}
                                 |Statistical Complexity: ${machine.statisticalComplexity}
                                 |Entropy Rate: ${machine.entropyRate}
                                 |Variation: ${machine.variation}
                                 |Number of Inferred States: ${allStates.states.length}\n
                                 |""".stripMargin

  val stateDetails: String = allStates.states
    .view
    .zipWithIndex
    .map {
      case (eqClass, i) =>
        val transitions = allStates.transitions(i)
          .map{ case (c, s) => c -> s.flatMap{ s=> Option("State " + allStates.stateMap(s))} }

        s"State $i:\n" +
          eqClass.histories.toArray.sortBy(_.observed).map{_.toString}.mkString("\n") +
          s"""
             |Probability Dist: ${eqClass.distribution.toString()}
             |  Frequency Dist: ${eqClass.frequency.toString()}
             |        Alphabet: ${alphabet.toString}
             |     transitions: $transitions
             |        P(state): ${allStates.distribution(i)}
             |""".stripMargin
    }
    .mkString("\n")

  protected def outStream(dataFile:File = null, fileName:String = "_out"):OutputStream = Option(dataFile) match {
    case Some(name) => new FileOutputStream(new File(dataFile.getAbsolutePath + fileName) )
    case None => System.out
  }

  protected def saferWriteOp(out:OutputStream, writer:(PrintWriter)=>Unit) = {
    val pw = new PrintWriter(out)
    try writer(pw) finally {
      if (System.out.eq(out)) pw.flush() else pw.close()
    }
  }

  def out(file:File = null):Unit = {
    saferWriteOp(outStream(file, "_info"), (measurementsOut) => {
      measurementsOut.print(metadata)
      measurementsOut.print(measurements)
    })

    saferWriteOp(outStream(file, "_inf.dot"), (dotOut) => dotOut.print(dotInfo) )

    saferWriteOp(outStream(file, "_results"), (statesOut) => statesOut.print(stateDetails) )

    // val statesOut = outStream(file, "_state_series")
  }
}
