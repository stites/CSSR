package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.measure.out.Results
import com.typeclassified.hmm.cssr.state.{AllStates, Machine, EquivalenceClass}
import com.typeclassified.hmm.cssr.test.Test
import com.typeclassified.hmm.cssr.parse.{Leaf, AlphabetHolder, Alphabet, Tree}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.io.{BufferedSource, Source}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object CSSR {
  protected val logger = Logger(LoggerFactory.getLogger(CSSR.getClass))

  def run(config: Config) = {
    logger.info("CSSR starting.")

    val (tree: Tree, allStates: ListBuffer[EquivalenceClass]) = initialization(config)
    logger.debug("Initialization complete...")

    sufficiency(tree, allStates, config.lMax, config.sig)
    logger.debug("Sufficiency complete...")

    destroyShortHistories(allStates, config.lMax)

    destroyOrphanStates(allStates, tree)

    recursion(tree, allStates, config.sig, config.lMax)

    logger.debug("Recursion complete...")

    destroyOrphanStates(allStates, tree)

    // TODO: This is very interesting:
    val sloop = allStates.map {
      s => s.histories.toList.map{
        h => h -> tree.alphabet.raw.map{
          c => c -> h.getRevLoopingStateOnTransitionTo(tree, allStates, c).flatMap{ e => Option(e.shortString) }
        }.toMap
      }
    }

    val transitions = allStates.map {
      s => s -> s.histories.map {
        h => tree.alphabet.raw.map {
          c => c -> h.getRevLoopingStateOnTransitionTo(tree, allStates, c)
        }.toMap
      }.head
    }.toMap

    val finalStates = new AllStates(allStates, transitions)

    val machine = new Machine(finalStates, tree)

    Results.metadata(config).split("\n").foreach(logger.info(_))
    Results.stateDetails(finalStates, AlphabetHolder.alphabet).split("\n").foreach(logger.info(_))
    Results.dotInfo(config, AlphabetHolder.alphabet, finalStates).split("\n").foreach(logger.info(_))
    Results.measurements(AlphabetHolder.alphabet, tree, machine, finalStates).split("\n").foreach(logger.info(_))

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
      recursive = true
      for (s <- S) {
        for ((b, alphabetIdx) <- parseTree.alphabet.map) {
          val x0 = s.histories.headOption
          val optionalTsb = x0.flatMap { l => l.getRevLoopingStateOnTransitionTo(parseTree, S, b) }

          if (x0.nonEmpty && optionalTsb.nonEmpty) {
            // TODO: investigate partitioning the equivalence class like so:
            // val nextRevLoopingStates:Map[Option[State], mutable.LinkedHashSet[Leaf]] = s.histories.tail
            //     .groupBy(_.getRevLoopingStateOnTransitionTo(parseTree, S, b))
            // val transitionStates = nextRevLoopingStates.keySet.flatten.filterNot(_.ne(s))

            for (x <- s.histories.tail) {
              val optionalExb = x.getRevLoopingStateOnTransitionTo(parseTree, S, b)
              if (optionalExb.nonEmpty && optionalTsb.get.ne(optionalExb.get)) {
                recursive = false
                for (y <- s.histories) {
                  val optionalEyb = y.getRevLoopingStateOnTransitionTo(parseTree, S, b)
                  if (optionalEyb.nonEmpty && optionalEyb.get.eq(optionalExb.get)) {
                    val sNew = EquivalenceClass()
                    S += sNew
                    Test.move(y, s, null, sNew, rmParent = true)
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
    S.foreach{ s => s.histories --= s.histories.filter(_.length < lMax - 1 )}
    S --= S.filter(_.histories.isEmpty)
  }

  type HistoryTransitions = Map[Leaf, Option[State]]
  type StateTransitions = Map[Char, HistoryTransitions]
  type AllStateTransitions = Map[State, StateTransitions]
  type StateToStateTransitions = Map[State, Map[Char, Option[State]]]

  def getHistoryTransitions(histories:Iterable[Leaf], transitionSymbol:Char, S:States, tree: Tree): HistoryTransitions = {
    histories.map { h => h -> h.getRevLoopingStateOnTransitionTo(tree, S.to[ListBuffer], transitionSymbol) }.toMap
  }

  def getStateTransitions(state:State, S:States, tree: Tree): StateTransitions = {
    tree.alphabet.raw.map { c => { c -> getHistoryTransitions(state.histories, c, S, tree) } }.toMap
  }

  def getAllStateTransitions(S:States, tree: Tree): AllStateTransitions = {
    S.map{ state => state -> getStateTransitions(state, S, tree) }.toMap
  }

  def getStateToStateTransitions(S:States, tree: Tree) :StateToStateTransitions = {
    S.map{ state => {
       state -> tree.alphabet.raw.map { c => {
         val transitions = state.histories.map { h => h.getRevLoopingStateOnTransitionTo(tree, S.to[ListBuffer], c) }.toSet.flatten
         if (transitions.size > 1) {
           throw new RuntimeException("This method should only be called once transitions has been determinized")
         } else {
           c -> (if (transitions.size == 1) Option(transitions.head) else None)
         }
       } }.toMap
    } }.toMap
  }

  def getStateToStateTransitionsShort(S:States, tree: Tree) :StateToStateTransitions = {
    S.map{ state => {
      state -> tree.alphabet.raw.map { c => {
        val transitions = state.histories
          .filter(_.length < tree.maxLength)
          .map { h => h.getRevLoopingStateOnTransitionTo(tree, S.to[ListBuffer], c) }.toSet.flatten
        if (transitions.size > 1) {
          // FIXME: check out what this means.
          throw new RuntimeException("This is an unknown scenario. Please email the maintainers.")
        } else {
          c -> (if (transitions.size == 1) Option(transitions.head) else None)
        }
      } }.toMap
    } }.toMap
  }


  def getStateToStateTransitionsLong(S:States, tree: Tree) :StateToStateTransitions = {
    S.map{ state => {
      state -> tree.alphabet.raw.map { c => {
        val transitions = state.histories
          .filter(_.length == tree.maxLength)
          .map { h => h.getRevLoopingStateOnTransitionTo(tree, S.to[ListBuffer], c) }.toSet.flatten
        c -> (if (transitions.size == 1) Option(transitions.head) else None)
      } }.toMap
    } }.toMap
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

    val shortTransitions = getStateToStateTransitionsShort(S.toList, tree)
    val longTransitions = getStateToStateTransitionsLong(S.toList, tree)
    val finalTransitionSet:Set[EquivalenceClass] = shortTransitions.foldLeft(Set[EquivalenceClass]()){
      case (memo, (_, charMap)) => memo ++ charMap.values.flatten.toSet
    }

    val foundRecurrStateArray = findRecurrStateArray(tree, S.toList)
    val possibleTransients = S.filterNot(foundRecurrStateArray._1.contains)

    // remove state only if state doesn't have a max length history with "unique characteristics"
    val transients = possibleTransients
      .filter { state => {
        // remove state only if a max length history loops in on itself
        lazy val noLongHistories = !state.histories.foldRight(false){ (h, hasLong) =>  hasLong || h.length == tree.maxLength }
        lazy val cycles = longTransitions(state).exists{ case (_, transition) => transition.nonEmpty && transition.get.ne(state)}
        lazy val notUnique = false                       // something to prove uniqueness (see below)
        noLongHistories || cycles || notUnique
      } }

    /*
      // AllStates.cpp#RemoveTransientStates, ln.966
      //check longest histories for uniqueness of transitions, if unique, don't delete
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

    S --= transients
  }

  def findRecurrStateArray(tree:Tree, S:States):(States, TransitionMemo) = {
    S.foldRight[(States, TransitionMemo)]((List(), Map())) {
      case (state, (recurrentStateMemo, transTableMemo)) =>
        fillRecurrStateArray(state, tree, S, recurrentStateMemo, transTableMemo)
    }
  }


  // Function: AllStates::FillRecurrStateArray
  // Purpose: fills an array with the states which are recurrent from
  //          particular state which is examined
  // In Params: alphabet, maximum length of history, index of specific
  //            state to be examined
  // Out Params: none
  // In/Out Params: array of recurrent states, number of states which are
  //                recurrent, table of transitons from max length strings
  // Pre- Cond: Memory has been allocated for the array of recurrent states
  // Post-Cond: The array of recurrent state indices has been set for the
  //            specific state (array of 'childstates' of given state)
  //////////////////////////////////////////////////////////////////////////
  type TransitionMemo = Map[String, (ParentState, Option[State])]
  def fillRecurrStateArray(state:State, tree:Tree, S:States, recurrentStateArray:States, transitionMemo: TransitionMemo) = {
    val histories = state.histories
    var found = false
    val stateArray = recurrentStateArray.to[ListBuffer]
    val transitionTable = transitionMemo.to[ArrayBuffer]

    histories.foreach {
      h => {
        val length = h.length
        for (c <- tree.alphabet.raw) {
          val tState = h.getRevLoopingStateOnTransitionTo(tree, S.to[ListBuffer], c)

          if (tState.nonEmpty) {
            val ts = tState.get
            if (ts.ne(state) && (length <= tree.maxLength - 1 || h.eq(histories.head))) {
              if (!stateArray.contains(ts)) {
                stateArray += ts
              }
            }
          }
          if (length == tree.maxLength) {
            transitionTable += (h.observed + c -> (state, tState))
          }
        }
      }
    }
    (stateArray.toList, transitionTable.toMap)
  }

}

