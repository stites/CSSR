package com.typeclassified.hmm.cssr

import scala.collection.mutable.ArrayBuffer
import breeze.linalg._

object EquivalenceClass {
  def apply() = new EquivalenceClass()
}

class EquivalenceClass extends Probablistic {
  var histories: ArrayBuffer[CausalState] = ArrayBuffer()

  def addHistory(h: CausalState): Unit = {
    histories += h
    normalizeAcrossHistories()
  }

  def rmHistory(x: CausalState): Unit = {
    histories = histories.filter(y => y != x)
    normalizeAcrossHistories()
  }

  def normalizeAcrossHistories(): Unit = {
    frequency = histories.foldRight(frequency)((history, totalFreq) => totalFreq :+ history.frequency)

    totalCounts = frequency.foldRight(0d)(_+_).toInt

    distribution = if (totalCounts == 0) DenseVector.ones(frequency.length) else normalize(frequency)
  }
}

