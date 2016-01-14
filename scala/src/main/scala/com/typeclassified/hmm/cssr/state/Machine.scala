package com.typeclassified.hmm.cssr.state

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}
import com.typeclassified.hmm.cssr.state.{Machine => M}
import com.typeclassified.hmm.cssr.state.Machine.{StateTransitionMap, InferredDistribution}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Machine {
  protected val logger = Logger(LoggerFactory.getLogger(Machine.getClass))

  type InferredDistribution = Array[(Leaf, Double)]

  type StateTransitionMap = Array[Map[Char, Option[Int]]]

  type StateToAllHistories = Map[Option[EquivalenceClass], Array[Leaf]]

  def inferredDistribution(tree: Tree, machine: Machine):InferredDistribution = {
    val inferred = tree
      .getDepth(tree.maxLength)
      .map { h => (h , inferredHistoryProbability(h.observed, tree, tree.alphabet, machine) ) }

    logger.debug(s"inferred distribution total: ${inferred.map{_._2}.sum}")
    logger.debug(s"inferred distribution size: ${inferred.length}")
    inferred.map{ case (l, p) => (l.observed, p) }.foreach{ i => println(i) }

    inferred
  }

  def inferredHistoryProbability(history:String, tree: Tree, alphabet: Alphabet, machine: Machine): Double = {
    // FIXME: this would be perfect to replace with a state monad
    logger.info("Generating Inferred probabilities from State Machine")

    val totalPerString = machine.states.view.zipWithIndex.map {
      case (state, i) =>
        logger.debug(s"${history} - STATE ${i.toString} {frequency:${machine.distribution(i)}}")
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

              logger.debug(s"""{
                               |freq at current state: ${currentState.distribution(alphabetIdx)},
                               |j: $i, symbol: $c@$alphabetIdx,
                               |totalPerState: $totalPerState,
                               |totalPerStateCached: $totalPerStateCached,
                               |transitioning to: ${transitionStateIdx.get},
                               |}""".stripMargin.replace('\n', ' '))

              totalPerStateCached
            }
          }
        }
        logger.debug(s"final totalPerState: $totalPerState")
        machine.distribution(i) * totalPerState
    }.sum[Double]

    logger.debug(s"Final Frequency for History: $totalPerString")
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

  def variation(dist:InferredDistribution, adjustedDataSize:Double): Double = {
    dist.foldLeft[Double] (0d) { (total, pair) => {
      val (history:Leaf, inferredProbability:Double) = pair
      val historyProbability:Double = sum(history.frequency / adjustedDataSize)
      total + math.abs(historyProbability - inferredProbability)
    } }
  }

  def relativeEntropy(dist:InferredDistribution, adjustedDataSize:Double):Double = {
    logger.debug("Relative Entropy")
    logger.debug("===========================")

    // FIXME : this _should not be <0_ however calculations provide the contrary
    // when the generated, inferred probability is greater than the observed one - we find the added log-ratio is < 0
    // currently, we have too many of that for the even process. This also begs the question: should there _ever_ be
    // a calculation where the log-ratio is < 0, since it was permissible in the C++. It may be that this is not the case
    // since I believe we are using K-L distribution for conditional, empirical distributions (yes?)
    val relativeEntropy:Double = dist.foldLeft(0d) {
      case (incrementalRelEnt, (leaf, inferredProb)) =>
        val observedFrequency = leaf.totalCounts / adjustedDataSize
        logger.debug(s"${leaf.toString}")
        logger.debug(s"historyProb: $observedFrequency")

        // it seems to me that we should be checking if the inferred probability is > 0.
        // By virtue of this: should the conditional be flipped? Note: this makes the final rel entropy non-negative
//        if (inferredProb > 0){
//          val logRatio = math.log(inferredProb / observedFrequency) // note that math.log in scala is the natural log
//          val cacheRE = incrementalRelEnt + inferredProb * logRatio
        if (observedFrequency > 0){
          val logRatio = math.log(observedFrequency / inferredProb) // note that math.log in scala is the natural log
          val cacheRE = incrementalRelEnt + observedFrequency * logRatio
          logger.debug(s"inferredProb: $inferredProb")
          logger.debug(s"logRatio:$logRatio")
          logger.debug(s"incrementalRelEnt:$cacheRE")
          cacheRE
        } else {
//          logger.debug(s"NO AGGREGATION! dataProb: $inferredProb")
          logger.debug(s"NO AGGREGATION! dataProb: $observedFrequency")
          incrementalRelEnt
        }
    }

    relativeEntropy
  }
  // the frequency of occurence of the history with that particular alpha symbol
  protected def relativeEntropyRateForHistory(adjustedDataSize:Double, history: Leaf, inferredProb:Double, tree: Tree, machine: Machine):Double = {
    val relEntRateHistTotal:Double = tree.alphabet.raw.foldLeft(0d){
      (relEntRateHist, alpha) => {
        val histFreqByAlpha = history.frequency(tree.alphabet.map(alpha)) / history.totalCounts
        val (accumulatedInferredRatio, relEntRateAlpha) = relativeEntropyRateForHistoryByAlphabet(history, inferredProb, tree, machine, histFreqByAlpha, alpha)
        relEntRateHist + relEntRateAlpha
      }
    }

    val histProbability:Double = history.totalCounts / adjustedDataSize

    if (relEntRateHistTotal < 0) 0 else relEntRateHistTotal / histProbability
  }

  protected def relativeEntropyRateForHistoryByAlphabet(history:Leaf, inferredProb:Double, tree:Tree, machine: Machine, histFreqByAlpha:Double, alpha:Char)
  :(Double, Double) = {
    val childStringProb = inferredHistoryProbability(alpha + history.observed,  tree, tree.alphabet, machine)
    var inferredRatioMemo:Double = 0
    var inferredRatio:Double = 0
    var relEntRateAlpha:Double = 0

    if (histFreqByAlpha > 0) {
      if (inferredProb > 0) {
        inferredRatio = childStringProb / inferredProb
        inferredRatioMemo += inferredRatio
      } else {
        // TODO: fill this out formally, later
        logger.error("Optionthing disastrous just happened")
      }

      relEntRateAlpha = histFreqByAlpha *  math.log(histFreqByAlpha / inferredRatio)
    }

    (inferredRatio, relEntRateAlpha)
  }

  def relativeEntropyRate(dist:InferredDistribution, adjustedDataSize:Double, tree: Tree, machine: Machine):Double = {
    logger.debug("Relative Entropy Rate")
    logger.debug("===========================")

    val relativeEntropyRate:Double = dist.foldLeft(0d) {
      case (partialRelEntRate, (leaf, inferredProb)) =>
        partialRelEntRate + relativeEntropyRateForHistory(adjustedDataSize, leaf, inferredProb, tree, machine)
    }

    relativeEntropyRate
  }

  def findNthSetTransitions(states:Array[EquivalenceClass], maxDepth: Int, alphabet: Alphabet, fullStates:StateToAllHistories)
  :StateTransitionMap = {

    val transitions = states.map {
      equivalenceClass => {
        val startHistories = fullStates(Option(equivalenceClass)).filter(_.observed.length == maxDepth-1)
        val endHistories = startHistories.flatMap(_.children)
        endHistories.groupBy(_.observation)
          .mapValues[Option[Int]] {
          nextHistories => {
            val validNextStates = nextHistories.map(_.currentEquivalenceClass).filter(states.contains(_)).toSet
            if (validNextStates.size != 1) {
              None
            } else {
              Option(states.indexOf(validNextStates.head))
            }
          } }
      } }

    ensureFullTransitionMap(transitions, alphabet.raw)
  }

  protected def ensureFullTransitionMap(transitionMap:StateTransitionMap, symbolsToFill: Array[Char]):StateTransitionMap = {
    transitionMap.map {
      symbolsToFill.foldRight(_) {
        (alphabetChar, tMap) => if (tMap.keySet.contains(alphabetChar)) tMap else tMap + (alphabetChar -> None)
      }
    }
  }

  def allHistoriesByState (tree: Tree, states:Array[EquivalenceClass]):StateToAllHistories = {
    tree.collectLeaves()
      .foldLeft(mutable.Map[Option[EquivalenceClass], ArrayBuffer[Leaf]]()){
        (map, leaf) => {
          val eq = leaf.currentEquivalenceClass
          if (map.keySet.contains(Option(eq))) map(Option(eq)) += leaf
          else if (!states.contains(eq)) map(None) += leaf
          else map(Option(eq)) = ArrayBuffer(leaf)
          map
        }
      }.toMap.mapValues(_.toArray)
  }
}

class Machine (equivalenceClasses: ListBuffer[EquivalenceClass], tree:Tree) {
  // initialization
  val states:Array[EquivalenceClass]             = equivalenceClasses.toArray
  val stateIndexes:Array[Set[Int]]               = states.map{_.histories.flatMap{_.locations.keySet}}
  val statePaths:Array[Array[String]]            = states.map{_.histories.map{_.observed}.toArray}

  val frequency:DenseVector[Double]              = new DenseVector[Double](stateIndexes.map{_.size.toDouble})
  val distribution:DenseVector[Double]           = frequency :/ sum(frequency)

  val fullStates:M.StateToAllHistories           = M.allHistoriesByState(tree, states)
  val transitionsByStateIdx:M.StateTransitionMap = M.findNthSetTransitions(states, tree.maxLength, tree.alphabet, fullStates)

  // Requires context of the machine itself -> not ideal, but logical
  val inferredDistribution:M.InferredDistribution = M.inferredDistribution(tree, this)

  val variation:Double = M.variation(inferredDistribution, tree.adjustedDataSize)
  val relativeEntropy = M.relativeEntropy(inferredDistribution, tree.adjustedDataSize)
  val relativeEntropyRate = M.relativeEntropyRate(inferredDistribution, tree.adjustedDataSize, tree, this)
  val statisticalComplexity = "TBD"
  val entropyRate = "TBD"
}

