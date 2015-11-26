package com.typeclassified.cssr

import com.typeclassified.cssr.parse.{AlphabetHolder, ParseAlphabet}
import breeze.linalg._

import scala.collection.mutable.ListBuffer

object CSSR {
  val datasize = 1000
  val A = List('a', 'b')
  var alphabet:ParseAlphabet = ParseAlphabet(A)
  AlphabetHolder.alphabet = CSSR.alphabet
  var obs:ListBuffer[Char] = new ListBuffer[Char]()
  val Lmax = 5
  val sig = 0.7
  val emptyState = State(0.toChar)
  val allStates:List[State] = List(emptyState)

  1 to datasize foreach { i => obs += A(i%2) }

  def main(args: Array[String]) = {
    println(obs.toList)
  }
}

