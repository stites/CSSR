package com.typeclassified.hmm.cssr.measure

import breeze.linalg.DenseVector
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf}

object HistoryProbablities {
  /**
    * calculates the probability of all the max length strings in the
    * data based on the inferred machine
    *
    * @param states
    * @param maxLength
    */
  def calcStringProbs(states:Array[Leaf], maxLength:Int, S:Array[Leaf], alphabet:Alphabet) : DenseVector[Double] = {
    return DenseVector[Double](states.map(h => calcStringProb(h.observed, S, alphabet)))
  }

  /**
    * calculates the probability of a string in the data based on the inferred machine
    *
    * @param string
    * @param S
    * @param alphabet
    * @return
    */
  def calcStringProb(string:String, S:Array[Leaf], alphabet:Alphabet) = {
    var totalPerString:Double = 0
    //    var currentState:CausalState = _
    //    val index:Int = _
    var frequency: Double = 0
    var transition:Double = 0

    for (s <- S) {
      val startState = s
      var totalPerState:Double = 1
      frequency = s.totalCounts
      var currentState = startState
      var isNullTrans = false
      for (symbol:Char <- string if !isNullTrans) {
        val index = alphabet.map(symbol)
        //get transition probability from current state
        totalPerState = totalPerState * currentState.distribution(index)

        //make transition
        transition = s.distribution(index)
        if (transition == 0) {
          totalPerState = 0
          isNullTrans = true
        } else {
          currentState = S(index)
        }
      }
      totalPerString += frequency * totalPerState
    }
    totalPerString
  }

}
