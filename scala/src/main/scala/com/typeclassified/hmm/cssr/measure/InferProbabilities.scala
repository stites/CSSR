package com.typeclassified.hmm.cssr.measure

import com.typeclassified.hmm.cssr.parse.{Tree, Leaf}
import com.typeclassified.hmm.cssr.state.AllStates
import com.typesafe.scalalogging.LazyLogging

object InferProbabilities extends LazyLogging {

  type InferredDistribution = Array[(Leaf, Double)]

  /**
    * calculates the probability of all the histories up to length/depth indicated, based on a given allStates
    */
  def inferredDistribution(tree: Tree, depth:Int, allStates: AllStates):InferredDistribution = {
    val inferred = tree
      .getDepth(depth)
      .map { h => (h , inferredHistory(h.observed, tree, allStates) ) }

    logger.debug(s"inferred distribution total: ${inferred.map{_._2}.sum}")
    logger.debug(s"inferred distribution size: ${inferred.length}")
    inferred.map{ case (l, p) => (l.observed, p) }.foreach{ i => println(i) }

    inferred
  }

  /**
    * calculates the probability of a single, raw history (in string form) based on a given allStates and alphabet
    */
  def inferredHistory(history:String, tree: Tree, allStates: AllStates): Double = {
    // FIXME: this would be perfect to replace with a state monad
    val pedantic = false
    if (pedantic) logger.info("Generating Inferred probabilities from State Machine")

    val totalPerString = allStates.states
      .view
      .zipWithIndex
      .map {
        case (state, i) =>
          if (pedantic) logger.debug(s"$history - STATE ${i.toString} {frequency:${allStates.distribution(i)}}")
          var currentStateIdx = i
          var isNullState = false

          val historyTotalPerState = history
            .foldLeft(1d) {
            (characterTotalPerState, c) => {
              val currentState = allStates.states(currentStateIdx)
              val transitionState = allStates.transitions(currentStateIdx)(c)
              isNullState = isNullState || transitionState.isEmpty

              if (isNullState) {
                0d
              } else {
                currentStateIdx  = allStates.states.zipWithIndex.find(_._1 == transitionState.get).get._2
                val totalPerStateCached = characterTotalPerState * currentState.distribution(tree.alphabet.map(c))
                if (pedantic) {
                  logger.debug(s"""{
                    |freq at current state: ${currentState.distribution(c)}
                    |j: $i, symbol: $c
                    |historyTotalPerState: characterTotalPerState,
                    |totalPerStateCached: $totalPerStateCached
                    |}""".stripMargin.replace('\n', ' '))
                  }
                totalPerStateCached
              }
            }
          }
          if (pedantic) logger.debug(s"final historyTotalPerState: $historyTotalPerState")
          allStates.distribution(i) * historyTotalPerState
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
        val nextEqClassIdx = allStates.states.indexOf(next.get.currentEquivalenceClass)

        println(totalPerState, c, current.get.observed, next.get.observed, leaf.observed, nextEqClassIdx)

        current = next
        if (!allStates.states.contains(next.get.currentEquivalenceClass)) {
          0d // we let this 0-probability eliminate null states.
        } else {
          totalPerState * currentState.distribution(alphabet.map(c))
        }
      }
    }
    */
  }

}
