package com.typeclassified.hmm.cssr.machines

import java.io.PrintWriter
import java.nio.file.{Path, Files}

trait TestMachine {
  object Input extends Enumeration {
    type Input = Value
    val Alphabet, Data = Value
  }

  import Input._

  def scaffoldFiles(name:String, alphabet:String, fillOp:PrintWriter=>Unit):Map[Input, Path] = {
    Map(
      Alphabet -> generateAlphabetFile(name, alphabet),
      Data -> generateDataFile(name, fillOp)
    )
  }

  protected def generateAlphabetFile(name:String, alphabet:String):Path = {
    generateDataFile(name+"_alphabet", (pw)=> alphabet.foreach(pw.print) )
  }

  // FIXME: Think about making output go to stdout and use redirects to put in files.
  protected def generateDataFile(name:String, fillOp:PrintWriter=>Unit):Path = {
    val data = Files.createTempFile(name, "")
    val dataPW = new PrintWriter(data.toFile)
    fillOp(dataPW)
    dataPW.close()
    data
  }
}
