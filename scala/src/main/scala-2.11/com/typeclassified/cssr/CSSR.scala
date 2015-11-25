package com.typeclassified.cssr

import scala.collection.mutable.ListBuffer

class CSSR {
  val datasize = 1000
  val A = Array('a', 'b')
  var obs:ListBuffer[Char] = new ListBuffer[Char]()
  1 to datasize foreach { i => obs += A(i%2) }
  val Lmax = 5
  val sig = 0.7

}

object CSSR {
  def main(args: Array[String]) = {
    println(new CSSR().obs.toList)
  }
}

