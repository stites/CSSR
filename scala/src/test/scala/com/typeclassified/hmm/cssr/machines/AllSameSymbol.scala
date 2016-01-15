package com.typeclassified.hmm.cssr.machines

/**
  * Program which produces a long data-file consisting of a long repetition of the same symbol, unspaced.
  * To be used in conjunction with CSSR for state-inference.
  */
class AllSameSymbol (val symbol:Char, val repetitions:Int) extends TestMachine {
  val inputs = scaffoldFiles("allSameSymbol", symbol.toString, (pw) => for (_ <- 0 to repetitions) pw.print(symbol) )
}
