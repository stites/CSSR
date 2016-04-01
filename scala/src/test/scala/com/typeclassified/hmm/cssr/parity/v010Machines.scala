package com.typeclassified.hmm.cssr.parity

import java.io.File

import com.typeclassified.hmm.cssr.CSSR
import com.typeclassified.hmm.cssr.cli.Config
import org.scalatest.{FunSuite, Matchers}
import org.scalactic.TolerantNumerics


class v010Machines extends FunSuite with Matchers {
  val epsilon = 1e-4f

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  object Alphabet extends Enumeration {
    type Alphabet = Value
    val BINARY = Value("binary")
  }

  import Alphabet._

  val binaryAlphabet = alphabetFile(BINARY)


  def dataFile (folder:String, prefix:String): File = {
    new File(getClass.getResource(s"/test-machines/$folder/${prefix}_timeseq").toURI)
  }

  def alphabetFile (alphabet:Alphabet): File = {
    new File(getClass.getResource(s"/alphabets/${alphabet.toString}").toURI)
  }

  test("the even process") {
    val config = new Config(binaryAlphabet, dataFile("even-process", "EP"), 4, 0.001)
    val results = CSSR.run(config)

    results.machine.relativeEntropy === 0.000105397d
    results.machine.relativeEntropyRate === 6.86369e-05d
    results.machine.statisticalComplexity === 0.918629d
    results.machine.entropyRate === 0.666321d
    results.machine.variation === 0.0108816d
    results.allStates.states should have size 2
  }
}
