package com.typeclassified.hmm.cssr

import java.util

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

    destroyShortHistories(allStates, config.lMax)

    destroyOrphanStates(allStates, parseTree)

    recursion(parseTree, allStates, config.sig, config.lMax)

    logger.debug("Recursion complete...")

    destroyOrphanStates(allStates, parseTree)

    val finalStates = new AllStates(allStates, getAllStateTransitions(allStates.toList, parseTree))

    val machine = new Machine(finalStates, parseTree)

    Results.metadata(config).split("\n").foreach(logger.info(_))
    Results.stateDetails(finalStates, AlphabetHolder.alphabet).split("\n").foreach(logger.info(_))
    Results.dotInfo(config, AlphabetHolder.alphabet, finalStates).split("\n").foreach(logger.info(_))
    Results.measurements(AlphabetHolder.alphabet, parseTree, machine, finalStates).split("\n").foreach(logger.info(_))

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

    while (!recursive) {
      // clean out transient states as well?
      recursive = true
      for (s <- S) {
        // TODO: investigate partitioning the equivalence class like so
        // val stateGroupedByTransition:Map[Option[EquivalenceClass], ArrayBuffer[Leaf]] = s.histories
        //     .groupBy(_.getStateOnTransitionTo(b))
        for ((b, alphabetIdx) <- parseTree.alphabet.map) {

          val x0 = s.histories.headOption
          val optionalTsb = x0.flatMap { l => l.getRevLoopingStateOnTransitionTo(parseTree, S, b) }

          if (x0.nonEmpty && optionalTsb.nonEmpty) {

            val nextRevLoopingStates = s.histories.tail.groupBy(_.getRevLoopingStateOnTransitionTo(parseTree, S, b))

            for (x <- s.histories.tail) {
              val optionalExb = x.getRevLoopingStateOnTransitionTo(parseTree, S, b)
              if (optionalExb.nonEmpty && optionalTsb.get.ne(optionalExb.get)) {
                logger.debug("recursive set to false")
                recursive = false
                for (y <- s.histories) {
                  val optionalEyb = y.getRevLoopingStateOnTransitionTo(parseTree, S, b)
                  if (optionalEyb.nonEmpty && optionalEyb.get.eq(optionalExb.get)) {
                    val sNew = EquivalenceClass()
                    S += sNew
                    logger.debug("moving from Recursion")
                    Test.move(y, s, null, sNew, true)
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

  def destroyShortHistories(S:ListBuffer[EquivalenceClass], lMax:Double): Unit = {
    S.foreach{ s => s.histories --= s.histories.filter(_.observed.length < lMax - 1 )}
    S --= S.filter(_.histories.isEmpty)
  }

  type HistoryTransitions = Map[Leaf, Option[State]]
  type StateTransitions = Map[Char, HistoryTransitions]
  type AllStateTransitions = Map[State, StateTransitions]

  def getHistoryTransitions(histories:Iterable[Leaf], transitionSymbol:Char, S:States, tree: Tree): HistoryTransitions = {
    histories.map { h => h -> h.getRevLoopingStateOnTransitionTo(tree, S.to[ListBuffer], transitionSymbol) }.toMap
  }

  def getStateTransitions(state:State, S:States, tree: Tree): StateTransitions = {
    tree.alphabet.raw.map { c => { c -> getHistoryTransitions(state.histories, c, S, tree) } }.toMap
  }

  def getAllStateTransitions(S:States, tree: Tree): AllStateTransitions = {
    S.map{ state => state -> getStateTransitions(state, S, tree) }.toMap
  }

  type State = EquivalenceClass
  type ParentState = EquivalenceClass
  type TransitionState = EquivalenceClass
  type Count = Int
  type States = List[State]

  def destroyOrphanStates(S:ListBuffer[State], tree: Tree): Unit = {
    // transition table only records transitions from LONGEST HISTORIES

    // stateArray only records transitions from SHORT HISTORIES:
    // only checks to see if the transition exists.
    val finalTransitionSet:Set[EquivalenceClass] = getAllStateTransitions(S.toList, tree)
      .foldLeft(Set[EquivalenceClass]()){
        case (memo, (_, charMap)) => memo ++ charMap.values.toList.flatMap(ks => ks.values.toList).flatten.toSet
      }

    /*
      // AllStates.cpp#RemoveTransientStates, ln.966
      //check longest histories for uniqueness
      //of transitions, if unique, don't delete
      transTemp = transTable->WhichStrings(z - removeAdjust);
      while (transTemp && isUnique == false) {
        //go to state of transitioning, max length history and check it against other histories
        isUnique = CheckUniqueTrans(transTemp->stringPtr, z - removeAdjust, transTemp->state - removeAdjust, alpha);
        transTemp = transTemp->nextPtr;
      }

      //remove state only if state doesn't have a max length history with unique characteristics transitioning to it
      if (isUnique == false) {
        transTable->RemoveStringsAndState(z - removeAdjust, removeAdjust, lowest); //reset transition table
        tempString = m_StateArray[z - removeAdjust]->getStringList();              //remove strings in state
        while (tempString) {
          tempString2 = tempString;
          tempString = tempString->m_nextPtr;
          m_table->RemoveString(tempString2->m_string);
        }
        RemoveState(z - removeAdjust);               //remove state itself (also removes from hash table)
        if (removeAdjust == 0) {
          lowest = z;
        }
        removeAdjust++;                              //restructure the state numbers after removal
        done = false;
      }
    }
     */

    S --= S.filterNot(finalTransitionSet.contains)
  }
}

