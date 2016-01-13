package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.measure.out.Results
import com.typeclassified.hmm.cssr.state.{Machine, EquivalenceClass}
import com.typeclassified.hmm.cssr.test.Test
import com.typeclassified.hmm.cssr.parse.{Leaf, AlphabetHolder, Alphabet, Tree}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.io.{BufferedSource, Source}
import scala.collection.mutable.ListBuffer

object CSSR {
  protected val logger = Logger(LoggerFactory.getLogger(CSSR.getClass))

  def run(config: Config) = {
    logger.info("CSSR starting.")

    val (parseTree: Tree, allStates: ListBuffer[EquivalenceClass]) = initialization(config)
    logger.debug("Initialization complete...")

    sufficiency(parseTree, allStates, config.lMax, config.sig)
    logger.debug("Sufficiency complete...")

    recursion(parseTree, allStates, config.sig, config.lMax)
    logger.debug("Recursion complete...")

    val machine = new Machine(allStates, parseTree)

    Results.metadata(config).split("\n").foreach{logger.info(_)}
    Results.stateDetails(machine).split("\n").foreach{logger.info(_)}
    Results.dotInfo(config, AlphabetHolder.alphabet, allStates).split("\n").foreach{logger.info(_)}
    Results.measurements(AlphabetHolder.alphabet, machine).split("\n").foreach{logger.info(_)}

    logger.info("CSSR completed successfully!")
  }

  /**
    * In Initialization, specifically of the parse tree, we are
    * iterating through the different histories and generating
    * causal states which each contain a next-step probability
    * distribution.
    */
  def initialization(config: Config): (Tree, ListBuffer[EquivalenceClass]) = {
    val alphabetSrc: BufferedSource = Source.fromFile(config.alphabetFile)
    val alphabetSeq: Array[Char] = try alphabetSrc.mkString.toCharArray finally alphabetSrc.close()

    val dataSrc: BufferedSource = Source.fromFile(config.dataFile)
    val dataSeq: Array[Char] = try dataSrc.mkString.toCharArray finally dataSrc.close()

    val alphabet = Alphabet(alphabetSeq)
    AlphabetHolder.alphabet = alphabet
    val rootClass = EquivalenceClass()
    val parseTree = Tree.loadData(Tree(alphabet, rootClass), dataSeq, config.lMax)
    rootClass.addHistory(parseTree.root)
    val allStates = ListBuffer(rootClass)

    return (parseTree, allStates)
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
  def sufficiency(parseTree: Tree, S: ListBuffer[EquivalenceClass], lMax: Int, sig:Double):Unit = {
    for (l <- 0 to lMax) {
      logger.debug(s"Starting Sufficiency at L = $l")
      for (xt <- parseTree.getDepth(l)) {
        val s = xt.currentEquivalenceClass

        for ((a, alphaIdx) <- parseTree.alphabet.map) {
          // node in the parse tree with predictive dist
          val aXt = xt.findChildWithAdditionalHistory(a)
          s.normalizeAcrossHistories()
          if (aXt.nonEmpty) {
            Test.test(S, aXt.get, s, sig)
          }
        }
      }
    }
    logger.debug("States found at the end of Sufficiency: " + S.size.toString)
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
    * @param sig
    */
  def recursion (parseTree: Tree, S: ListBuffer[EquivalenceClass], sig:Double, lMax:Double) = {
    var recursive = false
    // remove equivalence classes with empty histories
    S --= S.filter(_.histories.isEmpty)

    while (!recursive) {
      // clean out transient states as well?
      recursive = true
      for (s <- S; (b, alphabetIdx) <- parseTree.alphabet.map) {
        if (s.histories.nonEmpty) {
          // TODO: investigate partitioning the equivalence class like so
          // val stateGroupedByTransition:Map[Option[EquivalenceClass], ArrayBuffer[Leaf]] = s.histories
          //     .groupBy(_.getStateOnTransitionTo(b))
          val x0: Leaf = s.histories.head
          val optionalTsb = x0.getStateOnTransitionTo(b)
          if (optionalTsb.nonEmpty && x0.distribution(alphabetIdx) <= 0) {
            for (x <- s.histories.tail) {
              val optionalExb = x.getStateOnTransitionTo(b)
              if (optionalExb.nonEmpty && optionalTsb.get != optionalExb.get) {
                recursive = false
                for (y <- s.histories) {
                  val optionalEyb = y.getStateOnTransitionTo(b)
                  if (optionalEyb.nonEmpty && optionalEyb.get == optionalExb.get) {
                    val sNew = EquivalenceClass()
                    S += sNew
                    logger.debug("moving from Recursion")
                    Test.move(y, s, sNew)
                  }
                }
              }
            }
          }
        }
      }
    }
    logger.debug("States found at the end of Recursion: " + S.size.toString)
  }
}

