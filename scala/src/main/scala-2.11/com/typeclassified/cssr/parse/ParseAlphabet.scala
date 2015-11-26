package com.typeclassified.cssr.parse

object ParseAlphabet {
  def apply(alphabetRaw: List[Char]) = new ParseAlphabet(alphabetRaw)
}

class ParseAlphabet (alphabetRaw:List[Char]) {
  var map = alphabetRaw.zipWithIndex.map { case (c:Char, i:Int)=> c->i } .toMap
  var size = alphabetRaw.size
}


