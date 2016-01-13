package com.typeclassified.hmm.cssr.state

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Machine {
  protected val logger = Logger(LoggerFactory.getLogger(Machine.getClass))

  def calculateInferredDistribution(histories:Array[Leaf], tree: Tree, alphabet: Alphabet, machine: Machine):Array[(Leaf, Double)] = {
    histories.map{ h => ( h , calculateInferredHistoryProbability(h, tree, alphabet, machine) ) }
  }

  def calculateInferredHistoryProbability(leaf: Leaf, tree: Tree, alphabet: Alphabet, machine: Machine): Double = {
    // FIXME: this would be perfect to replace with a state monad
    logger.info("Generating Inferred probabilities from State Machine")

    val totalPerString = machine.states.view.zipWithIndex.map {
      case (state, i) => {
        logger.debug(s"${leaf.observed} - STATE ${i.toString} {frequency:${machine.distribution(i)}}")
        val frequency = state.distribution
        var currentStateIdx = i
        var isNullState = false

        val totalPerState = leaf.observed.reverse.view.zipWithIndex.foldLeft[Double](1d){
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
      }
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

  def calculateVariation (parseTree:Tree, alphabet: Alphabet, machine: Machine) :Double = {
    val histories = parseTree.getDepth(parseTree.maxLength)

    val inferredDistribution:Array[(Leaf, Double)] = calculateInferredDistribution(histories, parseTree, alphabet, machine )

    inferredDistribution.foreach{ s => println(s.toString) }

    inferredDistribution
      .foldLeft[Double] (0d) { (total, pair) => {
        val (history:Leaf, inferredProbability:Double) = pair
        val historyProbability:Double = sum(history.frequency / parseTree.adjustedDataSize)
        total + math.abs(historyProbability - inferredProbability)
      } }
  }

  def findNthSetTransitions(states:Array[EquivalenceClass], maxDepth: Int, alphabet: Alphabet, fullStates:Map[Option[EquivalenceClass], Array[Leaf]])
  :Array[Map[Char, Option[Int]]] = {

    val transitions = states.map {
      equivalenceClass => {
        val startHistories = fullStates(Some(equivalenceClass)).filter(_.observed.length == maxDepth-1)
        val endHistories = startHistories.flatMap(_.children)
        endHistories.groupBy(_.observation)
          .mapValues[Option[Int]] {
          nextHistories => {
            val validNextStates = nextHistories.map(_.currentEquivalenceClass).filter(states.contains(_)).toSet
            if (validNextStates.size != 1) {
              None
            } else {
              Some(states.indexOf(validNextStates.head))
            }
          } }
      } }

    ensureFullTransitionMap(transitions, alphabet.raw)
  }

  protected def ensureFullTransitionMap(transitionMap:Array[Map[Char, Option[Int]]], symbolsToFill: Array[Char])
  :Array[Map[Char, Option[Int]]] = {
    transitionMap.map {
      symbolsToFill.foldRight(_) {
        (alphabetChar, tMap) => if (tMap.keySet.contains(alphabetChar)) tMap else tMap + (alphabetChar -> None)
      }
    }
  }

  def allHistoriesByState (tree: Tree, states:Array[EquivalenceClass])
  : Map[Option[EquivalenceClass], Array[Leaf]] = {
    tree.collectLeaves()
      .foldLeft(mutable.Map[Option[EquivalenceClass], ArrayBuffer[Leaf]]()){
        (map, leaf) => {
          val eq = leaf.currentEquivalenceClass
          if (map.keySet.contains(Some(eq))) map(Some(eq)) += leaf
          else if (!states.contains(eq)) map(None) += leaf
          else map(Some(eq)) = ArrayBuffer(leaf)
          map
        }
      }.toMap.mapValues(_.toArray)
  }
}

class Machine (equivalenceClasses: ListBuffer[EquivalenceClass], tree:Tree) {
  val states:Array[EquivalenceClass] = equivalenceClasses.toArray
  val stateIndexes:Array[Set[Int]] = states.map{_.histories.flatMap{_.locations.keySet}}
  val statePaths:Array[Array[String]] = states.map{_.histories.map(_.observed).toArray}
  val fullStates:Map[Option[EquivalenceClass], Array[Leaf]] = Machine.allHistoriesByState(tree, states)
  val transitionsByStateIdx:Array[Map[Char, Option[Int]]] = Machine.findNthSetTransitions(states, tree.maxLength, tree.alphabet, fullStates)
  val frequency:DenseVector[Double] = new DenseVector[Double](stateIndexes.map{_.size.toDouble})
  val distribution:DenseVector[Double] = frequency :/ sum(frequency)

  val variation:Double = Machine.calculateVariation(tree, tree.alphabet, this)
  val relativeEntropy = "TBD"
  val relativeEntropyRate = "TBD"
  val statisticalComplexity = "TBD"
  val entropyRate = "TBD"
}

