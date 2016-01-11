package com.typeclassified.hmm.cssr.state

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.hmm.cssr.parse.Leaf

import scala.collection.mutable.ListBuffer

class Machine (equivalenceClasses: ListBuffer[EquivalenceClass]) {
  val states:Array[EquivalenceClass] = equivalenceClasses.toArray
  val stateIndexes:Array[Set[Int]] = states.map(_.histories.flatMap(_.locations.keySet))
  val statePaths:Array[Array[String]] = states.map(_.histories.map(_.observed).toArray)

  val frequency = new DenseVector[Double](stateIndexes.map(_.size.toDouble))
  val distribution:DenseVector[Double] = frequency :/ sum(frequency)


}
