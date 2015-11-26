package com.typeclassified.cssr

import com.typeclassified.cssr.parse.ParseNode

import scala.collection.mutable.ListBuffer

object State { def apply(c:Char) = new State(c) }

class State (val value:Char) extends Probablistic {
  var histories:ListBuffer[ParseNode] = ListBuffer()

  def addHistory (h:ParseNode)= {
    histories += h
    normalizeAcrossHistories()
  }

  def rmHistory (x:ParseNode) = {
    histories = histories.filter( y => y != x)
    normalizeAcrossHistories()
  }

  def normalizeAcrossHistories() = {
    frequency = histories.foldRight(frequency)((history, totalFreq) => totalFreq :+ history.frequency)

    totalCounts = frequency.foldRight(0d)(_+_).toInt

    normalDistribution = frequency:/(totalCounts)
  }

}

