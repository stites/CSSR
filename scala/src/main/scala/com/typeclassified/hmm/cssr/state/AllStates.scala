package com.typeclassified.hmm.cssr.state

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.hmm.cssr.CSSR.AllStateTransitions
import com.typeclassified.hmm.cssr.parse.Leaf

import scala.collection.mutable.ListBuffer

class AllStates (eqClasses:ListBuffer[EquivalenceClass], transitionMap:AllStateTransitions) {
  val states      = eqClasses.toArray
  val transitions = states.map{ state => transitionMap(state) }

  val stateIndexes:Array[Set[Int]] = states.map{_.histories.toSet[Leaf].flatMap{_.locations.keySet}}
  val statePaths:Array[Array[String]]     = states.map{_.histories.map{_.observed}.toArray}

  val frequency:DenseVector[Double]    = new DenseVector[Double](stateIndexes.map{_.size.toDouble})
  val distribution:DenseVector[Double] = frequency :/ sum(frequency)
}
