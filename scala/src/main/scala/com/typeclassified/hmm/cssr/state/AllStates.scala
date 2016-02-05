package com.typeclassified.hmm.cssr.state

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.hmm.cssr.CSSR.StateToStateTransitions

import scala.collection.mutable.ListBuffer

class AllStates (eqClasses:ListBuffer[EquivalenceClass], val transitionMap:StateToStateTransitions) {
  val states      = eqClasses.toArray
  val transitions = states.map{ state => transitionMap(state) }

  val stateIndexes:Array[Set[Int]]    = states.map{_.histories.flatMap{_.locations.keySet}.toSet}

  val frequency:DenseVector[Double]    = new DenseVector[Double](stateIndexes.map{_.size.toDouble})
  val distribution:DenseVector[Double] = frequency :/ sum(frequency)
}
