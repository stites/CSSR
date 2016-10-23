package com.typeclassified.hmm.cssr.measure.out

import java.io._

import _root_.com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.CSSR.TransitionState
import com.typeclassified.hmm.cssr.parse.Alphabet
import com.typeclassified.hmm.cssr.state.{AllStates, Machine}
import com.typeclassified.hmm.cssr.trees.ParseTree

class Results ( val config: Config,
                val alphabet: Alphabet,
                val tree:ParseTree,
                val machine: Machine,
                val allStates: AllStates,
                val parseString:String,
                val loopingString:String,
                val refinedString:String,
                stateLabels: Boolean
              ) {

  val metadata:String =
    s"""Metadata
        |=======================
        |${config.toString}
        |""".stripMargin

  protected val dotMeta:String =
    s"""digraph "${config.dataFile.getCanonicalPath}" {
        |size = \"6,8.5\";
        |ratio = \"fill\";
        |node [shape = circle];
        |node [fontsize = 24];
        |edge [fontsize = 24];
        |""".stripMargin

  def idxAsStr(i:Int):String = {
    if (stateLabels) {
      String.valueOf(i).map(c => (c.toInt + 17).toChar)
    } else {
      String.valueOf(i)
    }
  }

  val dotInfo: String = dotMeta + allStates.states
    .zipWithIndex
    .map {
      case (state, i) =>
        val sTransitions:Map[Char, TransitionState] = allStates.transitionMap(state)
        state.distribution
          .toArray
          .view.zipWithIndex
          .foldLeft[String]("") {
          case (memo, (prob, k)) if prob <= 0 => memo
          case (memo, (prob, k)) =>
            val symbol:Char = alphabet.raw(k)
            sTransitions(symbol) match {
              case None => memo
              case Some(transition) =>
                memo + s"""${idxAsStr(i)} -> ${idxAsStr(allStates.stateMap(transition))} [label = "$symbol: ${"%.7f".format(prob)}"];\n"""
            }
        }
    }
    .reduceLeft(_+_) + "}\n\n"

  val measurements: String =
    s"""Results
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
          .map{ case (c, s) => c -> s.flatMap{ s=> Option("State " + idxAsStr(allStates.stateMap(s)))} }

        s"State ${idxAsStr(i)}:\n" +
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

  protected def outStream(dataFile:File = null, fileName:String = "_out")
  :OutputStream = Option(dataFile) match {
    case Some(name) => new FileOutputStream(new File(dataFile.getAbsolutePath + fileName) )
    case None => System.out
  }

  protected def saferWriteOp(out:OutputStream, writer:(PrintWriter)=>Unit) = {
    val pw = new PrintWriter(out)
    try writer(pw) finally {
      if (System.out.eq(out)) pw.flush() else pw.close()
    }
  }

  def out(file:File = null):Results = {
    saferWriteOp(outStream(file, "_info"), (measurementsOut) => {
      measurementsOut.print(metadata)
      measurementsOut.print(measurements)
    })
    saferWriteOp(outStream(file, "_inf.dot"), (dotOut) => dotOut.print(dotInfo) )
    saferWriteOp(outStream(file, "_results"), (statesOut) => statesOut.print(stateDetails) )
    saferWriteOp(outStream(file, "_parsetree"), (statesOut) => statesOut.print(parseString) )
    saferWriteOp(outStream(file, "_loopingtree_pre_refinement"), (statesOut) => statesOut.print(loopingString) )
    saferWriteOp(outStream(file, "_loopingtree_post_refinement"), (statesOut) => statesOut.print(refinedString) )
    // val statesOut = outStream(file, "_state_series")

    this
  }
}
