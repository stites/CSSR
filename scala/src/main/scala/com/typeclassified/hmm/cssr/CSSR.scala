package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.measure.out.Results
import com.typeclassified.hmm.cssr.state.{AllStates, Machine, EquivalenceClass}
import com.typeclassified.hmm.cssr.test.Test
import com.typeclassified.hmm.cssr.parse.{Leaf, AlphabetHolder, Alphabet, Tree}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable
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

    val finalStates = recursion(parseTree, allStates, config.sig, config.lMax)
    logger.debug("Recursion complete...")

    val machine = new Machine(finalStates, parseTree)

    Results.metadata(config).split("\n").foreach{logger.info(_)}
    Results.stateDetails(finalStates, AlphabetHolder.alphabet).split("\n").foreach{logger.info(_)}
    Results.dotInfo(config, AlphabetHolder.alphabet, finalStates).split("\n").foreach{logger.info(_)}
    Results.measurements(AlphabetHolder.alphabet, parseTree, machine, finalStates).split("\n").foreach{logger.info(_)}

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

    (parseTree, allStates)
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

  // sufficiency is only good for 1 step
  // determinizing is further along than just 1 step.
  def sufficiency(parseTree: Tree, S: ListBuffer[EquivalenceClass], lMax: Int, sig:Double):Unit = {
    for (l <- 1 to lMax) {
      logger.debug(s"Starting Sufficiency at L = $l")
      for (xt <- parseTree.getDepth(l)) {
        val parent = parseTree.navigateHistoryRev(xt.observed.tail.toList) // FIXME: another atrocity. It's almost as if we are reading history in the _other direction_
        if (parent.nonEmpty) {
          val s = parent.get.currentEquivalenceClass
          s.normalizeAcrossHistories()
          Test.test(S, xt, parent.get, s, sig)
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

    destroyShortHistories(S, lMax)
    val trans = findLeTransitions(parseTree, S)
    val transients = findLeTransientsAndOrphans(trans, S)
//    cleanLeTransients(transients, S)

    while (!recursive) {
      // clean out transient states as well?
      recursive = true
      for (s <- S; (b, alphabetIdx) <- parseTree.alphabet.map) {
        // TODO: investigate partitioning the equivalence class like so
        // val stateGroupedByTransition:Map[Option[EquivalenceClass], ArrayBuffer[Leaf]] = s.histories
        //     .groupBy(_.getStateOnTransitionTo(b))

        val x0 = s.histories.headOption
        val optionalTsb = x0.flatMap{ l => l.getStateOnTransitionTo(b) }

        if (x0.nonEmpty && optionalTsb.nonEmpty) {
          for (x <- s.histories.tail) {
            val optionalExb = x.getLoopingStateOnTransitionTo(parseTree, b)
            if (optionalExb.nonEmpty && optionalTsb.get.ne(optionalExb.get)) {
              logger.debug("recursive set to false")
              recursive = false
              for (y <- s.histories) {
                val optionalEyb = y.getLoopingStateOnTransitionTo(parseTree, b)
                if (optionalEyb.nonEmpty && optionalEyb.get.eq(optionalExb.get)) {
                  val sNew = EquivalenceClass()
                  S += sNew
                  logger.debug("moving from Recursion")
                  Test.move(y, s, null, sNew)
                }
              }
            }
          }
        }
      }
    }

    val transI = findLeTransitions(parseTree, S)
    val transientsI = findLeTransientsAndOrphans(transI, S)
    cleanLeTransients(transientsI, S)

    logger.debug("States found at the end of Recursion: " + S.size.toString)
    new AllStates(S, transI)
  }


  def destroyShortHistories(S:ListBuffer[EquivalenceClass], lMax:Double): Unit = {
    S.foreach{ s => s.histories --= s.histories.filter(_.observed.length < lMax - 1 )}
    S --= S.filter(_.histories.isEmpty)
  }

  type Transitions = Map[EquivalenceClass, Map[Char, Option[EquivalenceClass]]]

  def findLeTransitions(tree: Tree, states:ListBuffer[EquivalenceClass]):Transitions = {
    tree.getDepth(tree.maxLength - 1)
      //      .filter{ _.currentEquivalenceClass }
      .groupBy{ _.currentEquivalenceClass }
      .mapValues{
        _.flatMap{ _.children }
          .groupBy{ _.observation }
          .mapValues{ histories => {
            val endStates = histories.flatMap{ h => Option(h.currentEquivalenceClass) }.filter{ states.contains(_) }.toSet
            if (endStates.size > 1) {
              logger.warn(s"Transition forks")
            }
            endStates.headOption
          } } }
      .mapValues{ charMap => tree.alphabet.raw.map{c => c -> charMap.getOrElse(c, None) }.toMap }
  }

  type Transients = Map[EquivalenceClass, Boolean]

  def findLeTransientsAndOrphans(transitions:Transitions, S:ListBuffer[EquivalenceClass]):Transients = {

    val transients = mutable.Map() ++ transitions.map {
      case (start, charToTransitions) =>
        val endStates = charToTransitions.values
        val isTransient = endStates.forall { _.isEmpty }
        val isOrphaned = endStates.flatten.forall { _ eq start }
        (start, isOrphaned || isTransient)
    }

    S.foreach{ s => {
      if (!transients.keySet.contains(s)) transients += (s -> true)
    } }

    transients.toMap
  }

  def cleanLeTransients(transients: Transients, S:ListBuffer[EquivalenceClass]):Unit = {
    S --= S.filter(transients(_))
  }
}

