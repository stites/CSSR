package com.typeclassified.hmm.cssr.state

import java.util.Optional

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Machine {

  def calculateInferredDistribution(histories:Array[Leaf], tree: Tree, alphabet: Alphabet, machine: Machine):Array[(Leaf, Double)] = {
    histories.map{ h => ( h , calculateInferredHistoryProbability(h, tree, alphabet, machine) ) }
  }

  def calculateInferredHistoryProbability(leaf: Leaf, tree: Tree, alphabet: Alphabet, machine: Machine)
  : Double = {
    // FIXME: this would be perfect to replace with a state monad
    var current:Option[Leaf] = Some(tree.root)

    leaf.observed.reverse.view.zipWithIndex.foldLeft[Double](1d){
      (totalPerState, pair) => {
        val (c, i) = pair
        val currentState = current.get.currentEquivalenceClass
        val next = current.get.findChildWithAdditionalHistory(c)

        println(totalPerState, c, current.get.observed, next.get.observed, leaf.observed)

        current = next
        if (!machine.states.contains(next.get.currentEquivalenceClass)) {
          0d // we let this 0-probability eliminate null states.
        } else {
          totalPerState * currentState.distribution(alphabet.map(c))
        }
      }
    }
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

  def findNthSetTransitions(states:Array[EquivalenceClass],
                            maxDepth: Int,
                            fullStates:Map[Optional[EquivalenceClass], Array[Leaf]])
  :Array[Map[Char, Optional[EquivalenceClass]]] = {
    states.map {
      equivalenceClass => {
        val startHistories = fullStates(Optional.of(equivalenceClass)).filter(_.observed.length == maxDepth-1)
        val endHistories = startHistories.flatMap(_.children)
        endHistories.groupBy(_.observation)
          .mapValues[Optional[EquivalenceClass]] {
          nextHistories => {
            val validNextStates = nextHistories.map(_.currentEquivalenceClass).filter(states.contains(_)).toSet
            if (validNextStates.size != 1) {
              Optional.empty()
            } else {
              Optional.of(validNextStates.head)
            }
          } }
      } }
  }

  def allHistoriesByState (tree: Tree, states:Array[EquivalenceClass])
  : Map[Optional[EquivalenceClass], Array[Leaf]] = {
    tree.collectLeaves()
      .foldLeft(mutable.Map[Optional[EquivalenceClass], ArrayBuffer[Leaf]]()){
        (map, leaf) => {
          val eq = leaf.currentEquivalenceClass
          if (map.keySet.contains(Optional.of(eq))) map(Optional.of(eq)) += leaf
          else if (!states.contains(eq)) map(Optional.empty()) += leaf
          else map(Optional.of(eq)) = ArrayBuffer(leaf)
          map
        }
      }.toMap.mapValues(_.toArray)
  }
}

class Machine (equivalenceClasses: ListBuffer[EquivalenceClass], tree:Tree) {
  val states:Array[EquivalenceClass] = equivalenceClasses.toArray
  val stateIndexes:Array[Set[Int]] = states.map{_.histories.flatMap{_.locations.keySet}}
  val statePaths:Array[Array[String]] = states.map{_.histories.map(_.observed).toArray}
  val fullStates:Map[Optional[EquivalenceClass], Array[Leaf]] = Machine.allHistoriesByState(tree, states)
  val transitions:Array[Map[Char, Optional[EquivalenceClass]]] = Machine.findNthSetTransitions(states, tree.maxLength, fullStates)
  val frequency:DenseVector[Double] = new DenseVector[Double](stateIndexes.map{_.size.toDouble})
  val distribution:DenseVector[Double] = frequency :/ sum(frequency)

  val variation:Double = Machine.calculateVariation(tree, tree.alphabet, this)
  println("variation", variation)
}

