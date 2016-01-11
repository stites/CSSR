package com.typeclassified.hmm.cssr.state

import java.util.Optional

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Machine {

  def calculateStringProbs (histories:List[Leaf], parseTree:Tree, alphabet: Alphabet, machine: Machine):List[Double] = {
    val stateMap:Map[EquivalenceClass, Int] = machine.states.toMap[EquivalenceClass, Int]

    histories.map { history => {
      machine.states.zipWithIndex.foldLeft[Double](0d) {
        case (totalPerString, (startState:EquivalenceClass, i:Int)) => {
          var totalPerState = 1d
          var currentState = startState
          var stateIndex = i
          var isNullTrans = false

          history.observed.toCharArray.foreach { c => {
            if (!isNullTrans) {
              totalPerState = totalPerState * currentState.distribution(alphabet.map(c))

              val transition: Optional[EquivalenceClass] = machine.transitions(stateIndex).getOrElse(c, Optional.empty())

              isNullTrans = !transition.isPresent
              if (isNullTrans) totalPerState = 0d
              else {
                currentState = transition.get()
                stateIndex = stateMap(currentState)
              }
            }
          } }
          totalPerState * machine.distribution(i)
        } }
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
}

