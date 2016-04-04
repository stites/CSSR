package com.typeclassified.hmm.cssr.shared

import java.io.File

trait FileHandlers {
  object Alphabet extends Enumeration {
    type Alphabet = Value
    val BINARY = Value("binary")
  }

  import Alphabet._

  val binaryAlphabet = alphabetFile(BINARY)

  def dataFile(folder: String, prefix: String): File = {
    new File(getClass.getResource(s"/test-machines/$folder/${prefix}_timeseq").toURI)
  }

  def alphabetFile(alphabet: Alphabet): File = {
    new File(getClass.getResource(s"/alphabets/${alphabet.toString}").toURI)
  }
}
