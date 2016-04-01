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

  val DEFAULT_SIG = 0.001

  def dataFile (folder:String, prefix:String): File = {
    new File(getClass.getResource(s"/test-machines/$folder/${prefix}_timeseq").toURI)
  }

  def alphabetFile (alphabet:Alphabet): File = {
    new File(getClass.getResource(s"/alphabets/${alphabet.toString}").toURI)
  }

  test("the even process") {
    val config = new Config(binaryAlphabet, dataFile("even-process", "EP"), 4, DEFAULT_SIG)
    val results = CSSR.run(config)

    results.machine.relativeEntropy === 0.000105397d
    results.machine.relativeEntropyRate === 6.86369e-05d
    results.machine.statisticalComplexity === 0.918629d
    results.machine.entropyRate === 0.666321d
    results.machine.variation === 0.0108816d
    results.allStates.states should have size 2
  }

  test("Misiurewics") {
    val config = new Config(binaryAlphabet, dataFile("misiurewicz", "Misiurewicz"), 4, DEFAULT_SIG)
    val results = CSSR.run(config)

    results.machine.relativeEntropy === 0d
    results.machine.relativeEntropyRate === 1.75591e-07d
    results.machine.statisticalComplexity === 2.78109d
    results.machine.entropyRate === 0.828277d
    results.machine.variation === 0.000218154d
    results.allStates.states should have size 8
  }
}
