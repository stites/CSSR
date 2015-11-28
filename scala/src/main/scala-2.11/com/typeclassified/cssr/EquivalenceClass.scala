package com.typeclassified.cssr

import com.typeclassified.cssr.parse.AlphabetHolder

import scala.collection.mutable.ArrayBuffer
import breeze.linalg._

object EquivalenceClass {
  def apply() = new EquivalenceClass()
}

class EquivalenceClass extends Probablistic {
  var histories: ArrayBuffer[CausalState] = ArrayBuffer()

  def addHistory(h: CausalState) = {
    histories += h
    normalizeAcrossHistories()
  }

  def rmHistory(x: CausalState) = {
    histories = histories.filter(y => y != x)
    normalizeAcrossHistories()
  }

  def normalizeAcrossHistories() = {
    frequency = histories.foldRight(frequency)((history, totalFreq) => totalFreq :+ history.frequency)

    totalCounts = frequency.foldRight(0d)(_+_).toInt

    distribution = if (totalCounts == 0) DenseVector.ones(frequency.length) else normalize(frequency)
  }
}

