package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.{Config, Cli}
import com.typeclassified.hmm.cssr.test.Test
import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, ParseAlphabet, ParseTree}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.io.{BufferedSource, Source}
import scala.collection.mutable.ListBuffer

object CSSR {
  val logger = Logger(LoggerFactory.getLogger(CSSR.getClass))
  var parseTree: ParseTree = _
  var lMax: Int = _
  var sig: Double = _
  var allStates: ListBuffer[EquivalenceClass] =_

  def main(args: Array[String]) = {
    Cli.parser.parse(args, Config()) match {
      case Some(config) => {
        initialization(config)
        sufficiency(parseTree, allStates, lMax)
        recursion(parseTree, allStates, lMax)
        logger.info("CSSR completed successfully!")
      }
      case None => {

      }
    }
  }

  /**
    * In Initialization, specifically of the parse tree, we are
    * iterating through the different histories and generating
    * causal states which each contain a next-step probability
    * distribution.
    */
  def initialization(config: Config): Unit = {
    lMax = config.lMax
    sig = config.sig

    val alphabetSrc: BufferedSource = Source.fromFile(config.alphabetFile)
    val alphabetSeq: Array[Char] = try alphabetSrc.mkString.toCharArray finally alphabetSrc.close()

    val dataSrc: BufferedSource = Source.fromFile(config.dataFile)
    val dataSeq: Array[Char] = try dataSrc.mkString.toCharArray finally dataSrc.close()

    AlphabetHolder.alphabet = ParseAlphabet(alphabetSeq)
    parseTree = ParseTree.loadData(dataSeq, lMax)
    allStates = ListBuffer(EquivalenceClass())
  }

  /**
    * In Sufficiency, we will step through the causal states and
    * generate equivalence classes (ie causal state machine)
    * containing the aggregate of next-step probability distributions
    * (sufficiency statistics) from the causal states.
    *
    * Equivalence classes will grow as a result of testing.
    *
    * @param parseTree
    * @param S
    * @param lMax
    */
  def sufficiency(parseTree: ParseTree, S: ListBuffer[EquivalenceClass], lMax: Int):Unit = {
    for (l <- 0 to lMax) {
      logger.debug(s"Starting Sufficiency at L = $l")
      for (xt <- parseTree.getDepth(l)) {
        val s = xt.currentEquivalenceClass
        for ((a, alphaIdx) <- AlphabetHolder.alphabet.map) {
          // node in the parse tree with predictive dist
          val aXt = xt.findChildWithAdditionalHistory(a)
          s.normalizeAcrossHistories()
          val p = s.distribution(alphaIdx)
          if (aXt.nonEmpty) {
            Test.test(S, p, aXt.get, s, sig)
          }
        }
      }
    }
    logger.info("States found at the end of Sufficiency: " + S.size.toString)
  }

  /**
    * In Recursion, we will verify that our equivalence classes are
    * correct. This is done by taking an equivalence class' causal
    * state (the first one is fine), adding new information to it
    * (from the alphabet), and comparing its probability distribution
    * (as a baseline) to all other causal states' estimates with this
    * new information.
    *
    * If there is a difference of the estimations from the baseline,
    * move the offending causal state to a new equivalence class.
    *
    * If there are no differences, we conclude.
    *
    * @param parseTree
    * @param S
    * @param lMax
    */
  def recursion (parseTree: ParseTree, S: ListBuffer[EquivalenceClass], lMax: Int) = {
    var recursive = false
    while (!recursive) {
      recursive = true
      for (s <- S) {
        for ((a, alphabetIdx) <- AlphabetHolder.alphabet.map) {
          val maybeX0 = s.histories.headOption
          if (maybeX0.nonEmpty) {
            val x0 = maybeX0.get
            val transitionToAEstimate = x0.distribution(alphabetIdx)
            for (x <- s.histories.tail) {
              val xTransitionToAEstimate = x.distribution(alphabetIdx)
              if (transitionToAEstimate != xTransitionToAEstimate) {
                val sNew = EquivalenceClass()
                S += sNew
                val newStateTransitionToA = xTransitionToAEstimate
                for (y <- s.histories if y.distribution(alphabetIdx) == newStateTransitionToA) {
                  Test.move(y, s, sNew)
                }
                recursive = false
              }
            }
          }
        }
      }
    }
    logger.info("States found at the end of Recursion: " + S.size.toString)
  }
}

