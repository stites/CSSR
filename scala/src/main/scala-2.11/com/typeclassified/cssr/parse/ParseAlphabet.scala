package com.typeclassified.cssr.parse

object ParseAlphabet {
  def apply(alphabetRaw: Array[Char]) = new ParseAlphabet(alphabetRaw)
}

class ParseAlphabet(val alphabetRaw: Array[Char]) {
  val map = alphabetRaw.zipWithIndex.map { case (c: Char, i: Int) => c -> i }.toMap
  val size = alphabetRaw.length
}

object AlphabetHolder {
  var alphabet: ParseAlphabet = _
}


