package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.{Config, Cli}
import com.typeclassified.hmm.cssr.test.Test
import com.typeclassified.hmm.cssr.parse.{Leaf, AlphabetHolder, Alphabet, Tree}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object CSSR {
  protected val logger = Logger(LoggerFactory.getLogger(CSSR.getClass))

  def main(args: Array[String]) = {
    Cli.parser.parse(args, Config()) match {
      case Some(config) => {
        logger.info("CSSR starting.\n")

        val (parseTree: Tree, allStates: ListBuffer[EquivalenceClass]) = initialization(config)
        logger.info("Initialization complete...")

        sufficiency(parseTree, allStates, config.lMax, config.sig)
        logger.info("Sufficiency complete...")

        logger.info("Statistics collected!\n")
        for ((histories, idx) <- collect(parseTree).zip(Stream from 1)) {
          println(s"State $idx:")
          histories.foreach(h => println(s"  $h"))
        }

        recursion(parseTree, allStates, config.sig)
        logger.info("Recursion complete...")

        for ((histories, idx) <- collect(parseTree).zip(Stream from 1)) {
          println(s"State $idx:")
          histories.foreach(h => println(s"  $h"))
        }

        logger.info("\nCSSR completed successfully!")
      }
      case None => { }
    }
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
    val parseTree = Tree.loadData(Tree(alphabet), dataSeq, config.lMax)
    val allStates = ListBuffer(EquivalenceClass())

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
    * @param sig
    */
  def recursion (parseTree: Tree, S: ListBuffer[EquivalenceClass], sig:Double) = {
    var recursive = false
    while (!recursive) {
      // clean out transient states
      recursive = true
      for (s <- S) {
        for ((b, alphabetIdx) <- parseTree.alphabet.map) {
          val maybeX0:Option[Leaf] = s.histories.headOption
          /*
           var x = s.histories.map(\ h->
              h.getStateOnTransition(b)
              var Exb:EquivalenceClass = null
              if (optionalExb.nonEmpty) {
                Exb = optionalTsb.get
              }
           )
           // => [(leaf, Equivclass)]
           parts = x.partition( /on equiv class/ )
           parts.len > 1 ?

           [ s*... ]

           */
          if (maybeX0.nonEmpty) {
            val x0:Leaf = maybeX0.get
            val optionalTsb = x0.getStateOnTransitionTo(b)
            var Tsb:EquivalenceClass = null
            if (optionalTsb.nonEmpty) {
              Tsb = optionalTsb.get
            }

            if (x0.distribution(alphabetIdx) <= 0) { logger.error("never get here") }
/// =========================
            for (x <- s.histories.tail) {

              val optionalExb = x.getStateOnTransitionTo(b)
              var Exb:EquivalenceClass = null
              if (optionalExb.nonEmpty) {
                Exb = optionalExb.get
              }

/// =========================
              if (Tsb != Exb) {
                recursive = false
                val sNew = EquivalenceClass()
                S += sNew
                for (y <- s.histories) {
                  val optionalEyb = y.getStateOnTransitionTo(b)
                  var Eyb:EquivalenceClass = null
                  if (optionalEyb.nonEmpty) {
                    Eyb = optionalEyb.get
                  }
                  if (Eyb == Exb) {
                    Test.move(y, s, sNew)
                  }
                }
              }
            }
          }
        }
      }
    }
    logger.info("States found at the end of Recursion: " + S.size.toString)
  }

  def collect (tree: Tree):Array[Array[String]] = {
    // more to come
    val leaves: Array[Leaf] = tree.collectLeaves()
    val statePartitions:mutable.HashMap[EquivalenceClass, ArrayBuffer[String]] = mutable.HashMap()
    leaves.foreach(l=>{
      if (statePartitions.contains(l.currentEquivalenceClass)) {
        statePartitions(l.currentEquivalenceClass) += l.observed
      } else {
        statePartitions(l.currentEquivalenceClass) = ArrayBuffer(l.observed)
      }
    })
    val x:ListBuffer[Array[String]] = ListBuffer()
    statePartitions.foreach { kvPair => x += kvPair._2.toArray }

    return x.toArray
  }
}

