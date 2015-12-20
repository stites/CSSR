package com.typeclassified.hmm.cssr.measure

import breeze.linalg.DenseVector
import com.typeclassified.hmm.cssr.parse.{Tree, Alphabet, Leaf}
import com.typeclassified.hmm.cssr.state.{EquivalenceClass, Machine}

object HistoryProbablities {
  /**
    * calculates the probability of all the max length strings in the
    * data based on the inferred machine
    *
    * @param states
    * @param maxLength
    */
  def calcStringProbs(states:Array[Leaf], maxLength:Int, S:Array[Leaf], alphabet:Alphabet) : DenseVector[Double] = {
//    return DenseVector[Double](states.map(h => calcStringProb(h.observed, S, alphabet)))
    DenseVector()
  }

  /**
    * calculates the probability of a string in the data based on the inferred machine
    *
    * @param string
    * @param machine
    * @return
    */
  def calcStringProb(string:String, machine:Machine, tree:Tree) = {
    //var totalPerString:Double = 0
    ////    var currentState:CausalState = _
    //var transition:Int = 0

    //for ((state, i) <- machine.states.view.zipWithIndex) {
    //  var totalPerState:Double = 1
    //  val startState = state
    //  var frequency: Double = machine.frequency(i)
    //  var currentState = startState
    //  var isNullTrans = false

    //  for (symbol:Char <- string if !isNullTrans) {
    //    val index = tree.alphabet.map(symbol)

    //    //get transition probability from current state
    //    totalPerState = totalPerState * currentState.distribution(index)

    //    //make transition
    //    transition = state.
    //    if (transition == 0) {
    //      totalPerState = 0
    //      isNullTrans = true
    //    } else {
    //      currentState = machine.states(transition)
    //    }
    //  }
    //  totalPerString += frequency * totalPerState
    //}
    //totalPerString
    0d
  }

  def collectTotalPerState(string: String, current:Leaf, totalPerState:Double, alphaMap: Map[Char, Int]):Double = {
    if (string.headOption.isEmpty) {
      return totalPerState
    } else {
      val c = string.head
      val mNext = current.findChildWithAdditionalHistory(c)
      if (mNext.isEmpty) {
        return totalPerState
      } else {
        val nextTransition = mNext.get.getStateOnTransitionTo(c)
        val nextTotal = if (nextTransition.nonEmpty) nextTransition.get.distribution(alphaMap(c)) * totalPerState else 0
        return collectTotalPerState(string.tail, mNext.get, nextTotal, alphaMap)
      }
    }
  }

}
