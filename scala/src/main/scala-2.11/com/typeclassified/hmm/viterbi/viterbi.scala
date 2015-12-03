package com.typeclassified.hmm.viterbi

import breeze.linalg.{*, DenseMatrix, DenseVector, argmax, max}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m}

object viterbi {
  def main (args:Array[String]): Unit = {
    val (healthy, fever) = ("Healthy", "Fever")
    val (normal, cold, dizzy) = ("normal", "cold", "dizzy")

    val states = Array(healthy, fever)
    val observations = Array(normal, cold, dizzy, cold, dizzy)
    val events = Array(normal, cold, dizzy)

    val startMap:Map[String, Double]= Map( healthy -> 0.6, fever -> 0.4 )
    val transitionMap:Map[String, Map[String, Double]]= Map(
      healthy -> Map(healthy -> 0.7, fever -> 0.3),
      fever -> Map(healthy -> 0.4, fever -> 0.6) )
    val emissionMap:Map[String, Map[String, Double]]= Map(
      healthy -> Map(normal -> 0.5, cold -> 0.4, dizzy -> 0.1),
      fever   -> Map(normal -> 0.1, cold -> 0.3, dizzy -> 0.6) )

    val mapResult = new viterbiMap(observations, states, events, startMap, transitionMap, emissionMap)
    print(mapResult)

    val startProbability = DenseVector(0.6, 0.4)
    val transitionProbs = DenseMatrix(
      (0.7, 0.3),
      (0.4, 0.6))
    val emissionProbs = DenseMatrix(
      (0.5, 0.4, 0.1),
      (0.1, 0.3, 0.6))
  }
}

class viterbiMap(obs:Array[String],
              states:Array[String],
              events:Array[String],
              start:Map[String, Double],
              trans:Map[String, Map[String, Double]],
              emit: Map[String, Map[String, Double]]
             ) {
  val V:ArrayBuffer[Map[String, Double]] = ArrayBuffer(Map())
  var path:m.HashMap[String, Array[String]] = m.HashMap()
  for (s <- states) {
    V(0) += (s -> start(s) * emit(s)(obs(0)))
    path += (s -> Array(s))
  }

  for (t <- 1 until obs.length) {
    V += Map()
    var newPath:m.HashMap[String, Array[String]] = m.HashMap()
    for (y <- states) {

      val (state, prob) = states.foldRight[Tuple2[String, Double]]((null, 0))((y0, sup) => {
        val test:Double = V(t-1)(y0) * trans(y0)(y) * emit(y)(obs(t))
        if (test > sup._2) (y0, test) else sup
      })

      V(t) += (y -> prob)
      newPath += (y -> (path(state)++Array(y)))
    }
    path = newPath
  }

  val n = obs.length - 1
  val (state, prob) = states.foldRight[Tuple2[String, Double]]((null, 0))((y, sup) => {
    if (V(n)(y) > sup._2) (y, V(n)(y)) else sup
  })

  val result = (prob, path(state))
}

class viterbiMatrix(obs:Array[String],
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

  for (t <- 1 to obs.length) {
    // V.append({})
    var newpath = List()

    val v_prev: DenseVector[Double] = v(t - 1)
    val em_t: DenseVector[Double] = emit(::, getIdx(obs(t)))
    println("em_t:", em_t)

    val x = trans.t(::, *) :* (v_prev :* em_t)

    val ms = max(x(::, *))
    println(ms)
    val a = argmax(x)
    println(a)

//      (prob, state) = max((V[t-1][y0] * trans_p[y0][y] * emit_p[y][obs[t]], y0) for y0 in states)
//      V[t][y] = prob
      val prob = v_prev
      v += prob
//      newpath[y] = path[state] + [y]

      /*
      // ========================
      for (s <- states) {
        val sIdx0 = states.indexOf(s)
        val vec: DenseVector[Double] = v(t - 1)(sIdx0) * trans(getIdx(obs(t)), sIdx) * emit(sIdx, getIdx(obs(t)))
      }
      val maxIdx:Int = argmax(vec)
      val maxProb:Double = vec(maxIdx)
      */
//    }
//    path = newpath
    println(v)
    println(path)

//    v += start :* emit(::, events.indexOf(obs.head))
//    val newPath: m.ListBuffer[List[String]] = m.ListBuffer()
//    for (s <- states) {
//      path(s) += s
//    }
//    path += states.toList
  }
}

