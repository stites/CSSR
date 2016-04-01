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
    results.machine.relativeEntropyRate === 5.68024e-06d
    results.machine.statisticalComplexity === 2.78325d
    results.machine.entropyRate === 0.828667d
    results.machine.variation === 0.00127077d
    results.allStates.states should have size 8
  }

  test("Foulkes") {
    val config = new Config(binaryAlphabet, dataFile("foulkes", "Foulkes"), 4, 0.01)
    val results = CSSR.run(config)

    results.machine.relativeEntropy === 0d
    results.machine.relativeEntropyRate === 0.000176433d
    results.machine.statisticalComplexity === 2.669d
    results.machine.entropyRate === 0.772877d
    results.machine.variation === 0.00585747d
    results.allStates.states should have size 7
  }
}
