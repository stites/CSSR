package com.typeclassified.hmm.cssr.parse

object ParseAlphabet {
  def apply(alphabetRaw: Array[Char]) = new ParseAlphabet(alphabetRaw)
}

class ParseAlphabet(val alphabetRaw: Array[Char]) {
  val map: Map[Char, Int]= alphabetRaw.zipWithIndex.map { case (c: Char, i: Int) => c -> i }.toMap
  val size: Int = alphabetRaw.length
}

object AlphabetHolder {
  var alphabet: ParseAlphabet = _
}


