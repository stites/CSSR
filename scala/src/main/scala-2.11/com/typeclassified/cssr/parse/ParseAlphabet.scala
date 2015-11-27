package com.typeclassified.cssr.parse

object ParseAlphabet {
  def apply(alphabetRaw: List[Char]) = new ParseAlphabet(alphabetRaw)
}

class ParseAlphabet (val alphabetRaw:List[Char]) {
  val map = alphabetRaw.zipWithIndex.map { case (c:Char, i:Int)=> c->i } .toMap
  val size = alphabetRaw.size
}

object AlphabetHolder {
  var alphabet:ParseAlphabet = _
}


