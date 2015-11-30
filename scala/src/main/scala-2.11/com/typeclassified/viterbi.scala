package com.typeclassified

import breeze.linalg.{argmax, DenseMatrix, DenseVector}

import scala.collection.{mutable => m}

object viterbi {
  def main (args:Array[String]): Unit = {
    val states = Array("Healthy", "Fever")
    val observations = Array("normal", "cold", "dizzy", "cold", "dizzy")
    val events = Array("normal", "cold", "dizzy")

    val startProbability = DenseVector(0.6, 0.4) // {'Healthy': 0.6, 'Fever': 0.4}
    val transitionProbs = DenseMatrix(
      (0.7, 0.3), // 'Healthy': {'Healthy': 0.7, 'Fever': 0.3},
      (0.4, 0.6)) // 'Fever': {'Healthy': 0.4, 'Fever': 0.6}

    val emissionProbs = DenseMatrix(
      (0.5, 0.4, 0.1), // 'Healthy': {'normal': 0.5, 'cold': 0.4, 'dizzy': 0.1},
      (0.1, 0.3, 0.6)) // 'Fever': {'normal': 0.1, 'cold': 0.3, 'dizzy': 0.6}

    new viterbi(observations, states, events, startProbability, transitionProbs, emissionProbs)
  }
}

class viterbi(obs:Array[String],
              states:Array[String],
              events:Array[String],
              start:DenseVector[Double],
              trans:DenseMatrix[Double],
              emit:DenseMatrix[Double]
             ) {
  def getIdx (str: String) = events.indexOf(str)

  val v: m.ListBuffer[DenseVector[Double]]  = m.ListBuffer()
  val path: m.HashMap[String, m.ListBuffer[String]] = m.HashMap()
  v += start :* emit(::, getIdx(obs.head))

  for (s <- states) {
    path += (s -> m.ListBuffer(s))
  }
  println(v)
  println(path)

  /*
  for (t <- 1 to obs.length) {
    for (s <- states) {
//      (prob, state) = max((V[t-1][y0] * trans_p[y0][y] * emit_p[y][obs[t]], y0) for y0 in states)
//      V[t][y] = prob
//      newpath[y] = path[state] + [y]
      val sIdx = states.indexOf(s)

      for (s <- states) {

        val sIdx0 = states.indexOf(s)
        val vec: DenseVector[Double] = v(t - 1)(sIdx0) * trans(getIdx(obs(t)), sIdx) * emit(sIdx, getIdx(obs(t)))
      }
      val maxIdx:Int = argmax(vec)
      val maxProb:Double = vec(maxIdx)
    }



    v += start :* emit(::, events.indexOf(obs.head))
    val newPath: m.ListBuffer[List[String]] = m.ListBuffer()
//    for (s <- states) {
//      path(s) += s
//    }
    path += states.toList
  }
  */

}
