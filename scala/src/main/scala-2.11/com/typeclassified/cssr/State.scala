package com.typeclassified.cssr

import com.typeclassified.cssr.parse.ParseNode

import scala.collection.mutable.ListBuffer

class State (val value:Char) extends Probablistic {
  var histories:ListBuffer[ParseNode] = ListBuffer()

  def addHistory (h:ParseNode)= {
    histories += h
    normalize_across_histories()
  }

  def rmHistory (x:ParseNode) = {
    histories = histories.filter( y => y != x)
    normalize_across_histories()
  }

  def normalize_across_histories() = {
    frequency = histories
      .map(parseNode => parseNode.frequency)
      .reduceRight((nodeFreq, accFreq) => nodeFreq :+ accFreq)(frequency)

    totalCounts = frequency.reduceRight(_+_).toInt

    normalDistribution = frequency :/ totalCounts
  }

}

object State { def apply(c:Char) = new State(c) }

