package com.typeclassified.hmm.cssr.parse

object Alphabet {
  def apply(alphabetRaw: Array[Char]) = new Alphabet(alphabetRaw)
}

class Alphabet(val alphabetRaw: Array[Char]) {
  val map: Map[Char, Int]= alphabetRaw.zipWithIndex.map { case (c: Char, i: Int) => c -> i }.toMap
  val size: Int = alphabetRaw.length
}

object AlphabetHolder {
  var alphabet: Alphabet = _
}


