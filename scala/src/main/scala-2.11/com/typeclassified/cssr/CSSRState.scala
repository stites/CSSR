package com.typeclassified.cssr

import com.typeclassified.cssr.parse.ParseNode

import scala.collection.mutable.ArrayBuffer

object CSSRState { def apply() = new CSSRState() }

class CSSRState extends Probablistic {
  var histories:ArrayBuffer[ParseNode] = ArrayBuffer()

  def addHistory (h:ParseNode) = {
    histories :+ h
    normalizeAcrossHistories()
  }

  def rmHistory (x:ParseNode) = {
    histories = histories.filter( y => y != x)
    normalizeAcrossHistories()
  }

  def normalizeAcrossHistories() = {
    frequency = histories.foldRight(frequency)((history, totalFreq) => totalFreq :+ history.frequency)

    totalCounts = frequency.foldRight(0d)(_+_).toInt

    normalDistribution = frequency :/ totalCounts.toDouble
  }
}

