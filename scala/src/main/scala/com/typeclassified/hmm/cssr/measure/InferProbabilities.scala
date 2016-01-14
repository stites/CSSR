package com.typeclassified.hmm.cssr.measure

import breeze.linalg.DenseVector
import com.typeclassified.hmm.cssr.parse.{Tree, Alphabet, Leaf}
import com.typeclassified.hmm.cssr.state.{EquivalenceClass, Machine}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

object InferProbabilities {

  protected val logger = Logger(LoggerFactory.getLogger(RelativeEntropy.getClass))

  type InferredDistribution = Array[(Leaf, Double)]

  /**
    * calculates the probability of all the histories up to length/depth indicated, based on a given machine
    */
  def inferredDistribution(tree: Tree, depth:Int, machine: Machine):InferredDistribution = {
    val inferred = tree
      .getDepth(depth)
      .map { h => (h , inferredHistory(h.observed, tree.alphabet, machine) ) }

    logger.debug(s"inferred distribution total: ${inferred.map{_._2}.sum}")
    logger.debug(s"inferred distribution size: ${inferred.length}")
    inferred.map{ case (l, p) => (l.observed, p) }.foreach{ i => println(i) }

    inferred
  }

  /**
    * calculates the probability of a single, raw history (in string form) based on a given machine and alphabet
    */
  def inferredHistory(history:String, alphabet: Alphabet, machine: Machine): Double = {
    // FIXME: this would be perfect to replace with a state monad
    //    logger.info("Generating Inferred probabilities from State Machine")

    val totalPerString = machine.states.view.zipWithIndex.map {
      case (state, i) =>
        //        logger.debug(s"${history} - STATE ${i.toString} {frequency:${machine.distribution(i)}}")
        val frequency = state.distribution
        var currentStateIdx = i
        var isNullState = false


        val totalPerState = history
          // TODO: IF WE ARE, INDEED, LOADING THE PARSE TREE INCORRECTLY THEN WE MUST REMOVE THIS LINE
          .reverse
          .view.zipWithIndex
          .foldLeft[Double](1d){
          (totalPerState, pair) => {
            val (c, i) = pair
            val currentState = machine.states(currentStateIdx)
            val transitionStateIdx = machine.transitionsByStateIdx(currentStateIdx)(c)
            isNullState = isNullState || transitionStateIdx.isEmpty
            val alphabetIdx = alphabet.map(c)

            if (isNullState) {
              0d
            } else {
              currentStateIdx = transitionStateIdx.get
              val totalPerStateCached = totalPerState * currentState.distribution(alphabet.map(c))

              /*
              logger.debug(s"""{
                               |freq at current state: ${currentState.distribution(alphabetIdx)},
                               |j: $i, symbol: $c@$alphabetIdx,
                               |totalPerState: $totalPerState,
                               |totalPerStateCached: $totalPerStateCached,
                               |transitioning to: ${transitionStateIdx.get},
                               |}""".stripMargin.replace('\n', ' '))
              */

              totalPerStateCached
            }
          }
        }
        //        logger.debug(s"final totalPerState: $totalPerState")
        machine.distribution(i) * totalPerState
    }.sum[Double]

    logger.debug(s"Final Probability for History: $totalPerString")
    totalPerString

    /*
    // TODO: ask about this: seems like we are just hitting a steady-state on every history. is this normal? if so, it looks like we are double-ish counting.
    leaf.observed.reverse.view.zipWithIndex.foldLeft[Double](1d){
      (totalPerState, pair) => {
        val (c, i) = pair

        val currentState = current.get.currentEquivalenceClass
        val next = current.get.findChildWithAdditionalHistory(c)
        val nextEqClassIdx = machine.states.indexOf(next.get.currentEquivalenceClass)

        println(totalPerState, c, current.get.observed, next.get.observed, leaf.observed, nextEqClassIdx)

        current = next
        if (!machine.states.contains(next.get.currentEquivalenceClass)) {
          0d // we let this 0-probability eliminate null states.
        } else {
          totalPerState * currentState.distribution(alphabet.map(c))
        }
      }
    }
    */
  }

}
