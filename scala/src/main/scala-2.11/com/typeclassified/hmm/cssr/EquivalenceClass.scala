package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, Leaf}

import scala.collection.mutable.ArrayBuffer
import breeze.linalg._

object EquivalenceClass {
  def apply() = new EquivalenceClass()
}

class EquivalenceClass extends Probablistic {
  var histories: ArrayBuffer[Leaf] = ArrayBuffer()

  def addHistory(h: Leaf): Unit = {
    histories += h
    normalizeAcrossHistories()
  }

  def rmHistory(x: Leaf): Unit = {
    histories = histories.filter(y => y.observed != x.observed)
    normalizeAcrossHistories()
  }

  def normalizeAcrossHistories(): Unit = {
    frequency = histories.foldRight(frequency)((history, totalFreq) => totalFreq :+ history.frequency)

    totalCounts = frequency.foldRight(0d)(_+_).toInt

    distribution = if (totalCounts == 0) DenseVector.zeros(frequency.length) else frequency / totalCounts
  }

  def collectHistories():Array[String] = {
    histories.map(_.observed).toArray
  }
}

