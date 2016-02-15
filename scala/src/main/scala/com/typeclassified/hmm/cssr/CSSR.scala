package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.measure.out.Results
import com.typeclassified.hmm.cssr.state.{AllStates, Machine, EquivalenceClass}
import com.typeclassified.hmm.cssr.test.Test
import com.typeclassified.hmm.cssr.parse.{Leaf, AlphabetHolder, Alphabet, Tree}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.collection.mutable.ListBuffer

object CSSR extends LazyLogging {
  // type aliases:
  type State = EquivalenceClass
  type ParentState = EquivalenceClass
  type TransitionState = Option[EquivalenceClass]
  type OrderedHistorySet = mutable.LinkedHashSet[Leaf]
  type States = List[State]
  type MutableStates = ListBuffer[State]

  type HistoryTransitions = Map[Leaf, TransitionState]
  type StateTransitions = Map[Char, HistoryTransitions]
  type AllStateTransitions = Map[ParentState, StateTransitions]
  type StateToStateTransitions = Map[ParentState, Map[Char, TransitionState]]
  type TransitionMemo = Map[String, (ParentState, TransitionState)]

  def run(config: Config) = {
    logger.info("CSSR starting.")

    val (tree: Tree, allStates: MutableStates) = initialization(config)
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
          c => c -> h.getTransitionState(tree, allStates, c).flatMap{ e => Option(e.shortString) }
        }.toMap
      }
    }

    val transitions = allStates.map {
      s => s -> s.histories.map {
        h => tree.alphabet.raw.map {
          c => c -> h.getTransitionState(tree, allStates, c)
        }.toMap
      }.head
    }.toMap

    val finalStates = new AllStates(allStates, transitions)

    val machine = new Machine(finalStates, tree)

    new Results(config, AlphabetHolder.alphabet, tree, machine, finalStates)
      .out(if (config.out) null else config.dataFile)

    logger.info("CSSR completed successfully!")
  }

  /**
    * In Initialization, specifically of the parse tree, we are
    * iterating through the different histories and generating
    * causal states which each contain a next-step probability
    * distribution.
    */
  def initialization(config: Config): (Tree, MutableStates) = {
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
  def sufficiency(parseTree: Tree, S: MutableStates, lMax: Int, sig:Double):Unit = {
    for (l <- 1 to lMax) {
      logger.debug(s"Starting Sufficiency at L = $l")
      for (xt <- parseTree.getDepth(l)) {
        // FIXME: another atrocity. It's almost as if we are reading history in the _other direction_
        val parent = parseTree.navigateHistoryRev(xt.observed.tail.toList)
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
  def recursion (parseTree: Tree, S: MutableStates, sig:Double, lMax:Double) = {
    var isDeterminized = false

    while (!isDeterminized) {
      isDeterminized = true
      for (s <- S) {
        for (b <- parseTree.alphabet.raw) {
          val x0 = s.histories.headOption
          val initialTransition = x0.flatMap { l => l.getTransitionState(parseTree, S, b) }

          if (x0.nonEmpty && initialTransition.nonEmpty) {
            s.histories
              .tail
              .groupBy(_.getTransitionState(parseTree, S, b))
              .filterKeys {
                case Some(eqClass) => eqClass.ne(initialTransition.get)
                case None => false
              }
              .foreach {
                case (_, histories) =>
                  isDeterminized = false
                  val sNew = EquivalenceClass()
                  S += sNew
                  // TODO: fix the signature for "when parent removal happens"
                  histories.foreach { h => Test.move(h, s, null, sNew, rmParent = true, paint = false) }
              }
          }
        }
      }
    }

    logger.debug("States found at the end of Recursion: " + S.size.toString)
  }

  def destroyShortHistories(S:MutableStates, lMax:Double): Unit = {
    S.foreach{ s => s.histories --= s.histories.filter(_.length < lMax - 1 )}
    S --= S.filter(_.histories.isEmpty)
  }

  def getHistoryTransitions(histories:Iterable[Leaf], transitionSymbol:Char, S:States, tree: Tree): HistoryTransitions = {
    histories.map { h => h -> h.getTransitionState(tree, S.to[ListBuffer], transitionSymbol) }.toMap
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
         val transitions = state.histories.map { h => h.getTransitionState(tree, S.to[ListBuffer], c) }.toSet.flatten
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
          .map { h => h.getTransitionState(tree, S.to[ListBuffer], c) }.toSet.flatten
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
          .map { h => h.getTransitionState(tree, S.to[ListBuffer], c) }.toSet.flatten
        c -> (if (transitions.size == 1) Option(transitions.head) else None)
      } }.toMap
    } }.toMap
  }

  def destroyOrphanStates(S:MutableStates, tree: Tree): Unit = {
    lazy val longTransitions = getStateToStateTransitionsLong(S.toList, tree)
    val recurStateArray = fillRecurrStateArray(tree, S.toList)
    val possibleTransients = S.filterNot(recurStateArray.contains)

    // remove state only if state doesn't have a max length history with "unique characteristics"
    // See old implementation: AllStates.cpp#RemoveTransientStates, ln.966
    val transients = possibleTransients
      .filter { state => {
        /* From previous version:
         *   - check longest histories for uniqueness of transitions, if unique, don't delete
         *   - go to state of transitioning, max length history and check it against other histories
         *   - remove state only if state doesn't have a max length history with unique characteristics
         *     transitioning to it
         */
        // remove state if there are no long histories (thus impossible to match uniqueness constraints)
        lazy val noLongHistories = !state.histories.foldRight(false){ (h, hasLong) =>  hasLong || h.length == tree.maxLength }
        // remove state if a max length history loops in on itself
        lazy val cycles = longTransitions(state).exists{ case (_, transition) => transition.nonEmpty && transition.get.ne(state)}
        // remove state if long history also matches uniqueness constraint
        lazy val notUnique = false  // FIXME: needs clarification
        noLongHistories || cycles || notUnique
      } }

    S --= transients
  }

  /**
    * fills an array with the states which are recurrent from particular state which is examined
    *
    * @return array of recurrent states and a table of transitions from max length strings
    */
  def fillRecurrStateArray(tree:Tree, S:States):States = {
    S.foldLeft[States](List()) {
      case (recurStateMemo, state) =>
        fillRecurrStateArray(state, tree, S, recurStateMemo)
    }
  }

  def fillRecurrStateArray(state:State, tree:Tree, S:States, recurrentStateArray:States) = {
    val histories = state.histories
    val stateArray = recurrentStateArray.to[ListBuffer]

    // stateArray only records transitions from short histories, there are none, in
    // which case use the first history
    histories.foreach { h =>
      tree.alphabet.raw.foreach { c =>
        val tState = h.getTransitionState(tree, S.to[ListBuffer], c).find(_.ne(state))
        val recordTransition = h.length <= tree.maxLength - 1 || histories.head.eq(h)

        if (tState.nonEmpty && recordTransition && !stateArray.contains(tState.get)) {
          stateArray += tState.get
        }
      }
    }
    stateArray.toList
  }

}

