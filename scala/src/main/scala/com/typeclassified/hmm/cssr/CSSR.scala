package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.measure.out.Results
import com.typeclassified.hmm.cssr.shared.Epsilon
import com.typeclassified.hmm.cssr.shared.{Level, Logging}
import com.typeclassified.hmm.cssr.state.{AllStates, Machine, State}
import com.typeclassified.hmm.cssr.parse.{Alphabet, AlphabetHolder}
import com.typeclassified.hmm.cssr.trees._

import scala.collection.{Set, mutable}
import scala.io.{BufferedSource, Source}
import scala.collection.mutable.ListBuffer

object CSSR extends Logging {
  override def loglevel() = Level.INFO

  // type aliases:
  type ParentState = State
  type TransitionState = Option[State]

  type StateToStateTransitions = Map[ParentState, Map[Char, TransitionState]]

  def run(config: Config):Results  = {
    implicit val ep:Epsilon = new Epsilon(0.01)

    val tree: ParseTree = initialization(config)
    val parseString = printParse(tree.root)
    val looping = grow(tree)
    val loopingString = printLooping(looping)
    refine(looping)
    val refinedString = printLooping(looping)

    val (allStates, transitions, stateMap) = statesAndTransitions(tree, looping)
    // with the looping tree "grown", we now need to collect histories within each state so that we can later examine our distribution

    collect(tree, looping, tree.maxLength, allStates.toSet, stateMap)
    allStates.foreach(_.pruneHistories(tree.maxLength))

    val finalStates = new AllStates(allStates, transitions)
    val machine = new Machine(finalStates, tree)

    new Results(config, AlphabetHolder.alphabet, tree, machine, finalStates, parseString, loopingString, refinedString, config.stateLabels)
      .out(if (config.out) null else config.dataFile)
  }

  def printParse(parseLeaf: ParseLeaf, nTabs:Int = 0, memo:String=""): String = {
    val acc = s"$memo${"\t" * nTabs}$parseLeaf\n"
    val children = parseLeaf.children.map(this.printParse(_, nTabs+1, "")).mkString("\n")
    acc + children
  }

  def printLooping(loopingTree: LoopingTree): String = {
    var s = "printing looping tree with terminals:\n"

    for(t <- loopingTree.terminals) {
      s += s"\t$t\n"
    }

    s += "looping tree:\n"

    def go(lLeaf: LoopingTree.Node, nTabs:Int, memo:String): String = {
      val tabs = "\t" * nTabs
      var _memo = memo

      lLeaf match {
        case Left(leaf) =>
          _memo += s"$tabs$leaf\n"
          _memo += leaf.children.values.map(go(_, nTabs+1, "")).mkString("\n")
          _memo
        case Right(Left(es))    => s"$memo$tabs$es"
        case Right(Right(loop)) => s"$memo$tabs$loop"
      }
    }

    s + go(Left(loopingTree.root), 1, "")
  }

  def statesAndTransitions(parse:ParseTree, looping: LoopingTree):(Iterable[State], StateToStateTransitions, Map[Terminal, State]) = {
    val stateMap:Map[Terminal, State] = looping.terminals.map{ t => t -> new State(t)}.toMap
    val allStates:Iterable[State] = stateMap.values
    val transitions = mapTransitions(looping, stateMap)
    (allStates, transitions, stateMap)
  }

  def pathToState(ltree: LoopingTree, terminal: Terminal, char: Char, stateMap:Map[Terminal, State]): Option[State] = {
    val wa = terminal.path().mkString + char
    if (terminal.distribution(ltree.alphabet.map(wa.last)) == 0) None else {
      ltree
        .navigateToTerminal(wa, ltree.terminals)
        .flatMap(t => if (stateMap.contains(t)) Option(stateMap(t)) else None)
    }
  }

  def mapTransitions(ltree:LoopingTree, stateMap: Map[Terminal, State]):StateToStateTransitions = {
    val mappings: Map[State, Map[Char, Option[State]]] = stateMap.toArray
      .map{ case (tNode, state) => {
        val tStates:Map[Char, Option[State]] = ltree
          .alphabet
          .raw
          .map { a => a -> pathToState(ltree, tNode, a, stateMap) }
          .toMap
        state -> tStates
      } }
      .toMap
    // primarily for debugging
    mappings
  }

  /**
    * Phase I: "Initialization":
    *
    * Requires estimates of conditional probabilities to converge, perhaps rapidly.
    */
  def initialization(config: Config):ParseTree = {
    val alphabetSrc: BufferedSource = Source.fromFile(config.alphabetFile)
    val alphabetSeq: Array[Char] = try alphabetSrc.mkString.toCharArray finally alphabetSrc.close()

    val dataSrc: BufferedSource = Source.fromFile(config.dataFile)
    val dataSeq: Array[Char] = try dataSrc.mkString.toCharArray finally dataSrc.close()

    val alphabet = Alphabet(alphabetSeq)
    AlphabetHolder.alphabet = alphabet
    val parseTree = ParseTree.loadData(ParseTree(alphabet), dataSeq, config.lMax)

    parseTree
  }

  /**
    * Phase II: "Growing a Looping Tree" algorithm:
    *
    * INIT root looping node
    * INIT queue of active, unchecked nodes
    * QUEUE root
    * WHILE queue is not empty
    *   DEQUEUE first looping node from the queue
    *   COMPUTE homogeneity(dequeued looping node, parse tree)
    *   IF node is homogeneous
    *   THEN continue
    *   ELSE
    *     CONSTRUCT new looping nodes for all valid children (one for each symbol in alphabet - must have empirical observation in dataset).
    *     FOR each new node constructed
    *       COMPUTE excisability(node, looping tree)
    *       COMPUTE isEdge(node, looping tree)
    *       ADD all new looping nodes to children of active node (mapped by symbol)
    *       ADD unexcisable children to queue (FIXME: what about edgesets?)
    *   ENDIF
    * ENDWHILE
    *
    * isEdge:
    *   INPUTS: looping node, looping tree
    *   COLLECT all terminal nodes that are not ancestors
    *   IF exists terminal nodes with identical distributions
    *   THEN
    *     mark looping node as an edge set
    *     mark found terminals as an edge set
    *     // We will merge edgesets in Phase III.
    *   ENDIF
    *
    * Homogeneity:
    *   INPUTS: looping node, parse tree
    *   COLLECT all next-step histories from looping node in parse tree
    *   FOR each history in next-step histories
    *     FOR each child in history's children
    *       IF child's distribution ~/=  node's distribution
    *       THEN RETURN false
    *       ENDIF
    *     ENDFOR
    *   ENDFOR
    *   RETURN TRUE
    *
    * Excisability:
    *   INPUTS: looping node, looping tree
    *   COLLECT all ancestors of the looping node from the looping tree, ordered by increasing depth (depth 0, or "root node," first)
    *   FOR each ancestor
    *     IF ancestor's distribution == looping node's distribution
    *     THEN
    *       the node is excisable: create loop in the tree
    *       ENDFOR (ie "break")
    *     ELSE do nothing
    *     ENDIF
    *   ENDFOR
    *
    * @param tree A parse tree containing the conditional probabilities of a time series
    * @return A looping tree representing the initial growth of a looping tree
    */
  def grow(tree:ParseTree):LoopingTree = {
    val ltree = new LoopingTree(tree)
    val activeQueue = ListBuffer[LLeaf](ltree.root)
    val findAlternative = LoopingTree.findAlt(ltree)(_)

    while (activeQueue.nonEmpty) {
      val active:LLeaf = activeQueue.remove(0)
      val isHomogeneous:Boolean = active.histories.forall{ LoopingTree.nextHomogeneous(tree) }

      if (isHomogeneous) {
        debug("we've hit our base case")
      } else {

        val nextChildren:Map[Char, LoopingTree.Node] = active.histories
          .flatMap { _.children }
          .groupBy{ _.observation }
          .map { case (c, pleaves) => {
            val lleaf:LLeaf = new LLeaf(c + active.observed, pleaves, Option(active))
            val alternative:Option[LoopingTree.AltNode] = findAlternative(lleaf)
            c -> alternative.toRight(lleaf)
          } }

        active.children ++= nextChildren
        // Now that active has children, it cannot be considered a terminal node. Thus, we elide the active node:
        ltree.terminals = ltree.terminals ++ LoopingTree.leafChildren(nextChildren).toSet[LLeaf] - active
        // FIXME: how do edge-sets handle the removal of an active node? Also, are they considered terminal?
        activeQueue ++= LoopingTree.leafChildren(nextChildren)
      }
    }

    ltree
  }

  /**
    * SET change = true
    * UNTIL change == false
    *   INIT transition map (terminal -> node)
    *
    *   // calculate predictive probabilities
    *   FOR each terminal node (t)
    *     FOR each non-looping path w to t
    *       FOR each symbol a in the alphabet follow the path wa in the tree
    *         IF wa leads to a terminal node
    *         THEN store terminal's transition in transition map
    *         ELSEIF wa does not lead to a terminal node
    *           copy the sub-looping-tree rooted at (the node reached by) wa to t, giving all terminal nodes the predictive distribution of t
    *           store terminal's transition in transition map
    *           - continue
    *         ELSE
    *           ENDFOR (break inner-most loop)
    *         ENDIF
    *       ENDFOR
    *     ENDFOR
    *   ENDFOR
    *
    *   // merge edgesets:
    *   COLLECT map of (terminal nodes -> edgesets)
    *     FILTER terminals in transition map keys where terminal.isEdgeSet == true
    *     GROUPBY a terminal's distribution and the value from the transition map
    *
    *
    *   FOR each terminal node belonging to an edgesets
    *     SET terminal.edgeset to the corresponding set from the map of (terminal nodes -> edgesets)
    *
    *   IF map of (terminal nodes -> edgesets) is nonEmpty
    *   THEN set change = true
    *   ENDIF
    *
    * ENDUNTIL
    *
    * Questions:
    *   + if we let a terminal node's distribution override another terminal node's distribution (via subtree) will order matter?
    */

  type wa = String
  type Terminal = LLeaf

  def stepFromTerminal(ltree: LoopingTree)(terminal: Terminal):Array[(Terminal, Option[LLeaf])] = {
    val w = terminal.path().mkString
    val alphabet = ltree.alphabet.raw

    def navigateToNext(alphaIdx:Int) = ltree.navigateLoopingTree(w + alphabet(alphaIdx))
    val paths = mutable.ArrayBuffer[(Terminal, Option[LLeaf])]()

    for ((i, p) <- terminal.distribution.activeIterator) {
      if (p > 0) {
        paths ++= Array((terminal, navigateToNext(i))) // kill me now
      }
    }

    paths.toArray
  }

  def refine(ltree:LoopingTree) = {
    var stillDirty = false

    do {
      val toCheck: Set[(Terminal, LLeaf)] = ltree.terminals
        .flatMap { stepFromTerminal(ltree)(_) }
        .filterNot { case (term, wa) => wa.isEmpty }
        .map{ case (term, wa) => (term, wa.get) }

      val transitions:mutable.Map[Terminal, Set[Terminal]] = mutable.Map()

      stillDirty = toCheck
        .foldLeft(false){
          case (isDirty:Boolean, (t:Terminal, wa:LLeaf)) => {
            // check to see if this leaf is _not_ a terminal leaf
            if (ltree.terminals.contains(wa)) {
              val tos = transitions.getOrElse(t, Set())
              transitions.put(t, tos + wa)
              isDirty
            } else {
              // if we find a "terminal-looping" node (ie- any looping node) that is not a terminal node:
              //   | if it loops to a terminal node: merge this node into the terminal node
              //   | else: make its value a terminal node
              // if we find a "terminal-edgeSet" node: merge this node into the terminal node
              //
              // if either of the above return None, else we have a sub-looping-tree and return Some.
              wa match {
                case loop: Loop =>
                  val alreadyRefined = loop.value.terminalReference.nonEmpty
                  if (alreadyRefined) {
                    // merge this node into the terminal node
                    val terminal = loop.value.terminalReference.get
                    terminal.addHistories(loop.histories)
                    loop.terminalReference = Some(terminal)
                    isDirty // no refinement needed, continue with current isDirty value
                  } else { // we have a non-terminating loop
                    // FIXME: merge the loop's empirical observations as well - but maybe we should do this above...
                    // ...at any rate, the loop is all we need to prototype this.
                    loop.value.terminalReference = Option(loop)
                    ltree.terminals ++= Set(loop)
                    ltree.collectLeaves(ListBuffer(wa))
                      .filterNot { ltree.terminals.contains } // terminal nodes cannot be overwritten
                      .foreach { _.refineWith(wa) }
                    true // loop is now dirty
                  }
                case _ =>
                  // refine subtree
                  if (wa.terminalReference.isEmpty) {
                    ltree.collectLeaves(ListBuffer(wa))
                      .filterNot { ltree.terminals.contains } // terminal nodes cannot be overwritten
                      .foreach { _.refineWith(t) }
                    true // loop is now dirty
                  } else {
                    isDirty
                  }
              }
            }
          }
        }

      val transitionGroups:Iterable[Set[Terminal]] = transitions
        // group by transitions
        .groupBy{ _._2 }
        // throw away transitions, look only at grouped terminals
        .mapValues(_.keySet)
        .values
        // split groups by matching distribution

      val toMerge = transitionGroups
        .flatMap {
          tSet => {
            val (head:Terminal, tail:List[Terminal]) = unsafeHeadAnd(tSet.toList.sortBy(_.observed))
            val newSet = mutable.Set(head)
            head.edgeSet = Some(newSet)

            // just in case there are actually multiple edgsets found with matching transitions
            val edgeSets:Set[Set[Terminal]] = tail.foldLeft(Set(newSet))((ess, t) => {
              var matchFound = false

              for (es <- ess if !matchFound && Tree.matches(t)(es.head)) {
                es += t
                t.edgeSet = Some(es)
                matchFound = true
              }

              if (matchFound) ess else {
                val newSet = mutable.Set(t)
                t.edgeSet = Some(newSet)
                ess ++ Set(newSet)
              }
            })
            .map(_.toSet)

            edgeSets
          }
        }
        // don't look at singleton groups
        .filter{ _.size > 1 }


      // perform final updates and merges
      toMerge
        .foreach {
          set:Set[Terminal] =>
            // holy moly we need to turn this into a dag and not a cyclic-linked-list-tree
            val (head:Terminal, tail:List[Terminal]) = unsafeHeadAnd(set.filter{_.parent.nonEmpty}.toList.sortBy(_.observed))
            tail.foreach {
              node =>
                node.parent.get.children.put(node.observation, Left(head))
                ltree.terminals = ltree.terminals - node
            }
        }

      stillDirty = stillDirty || toMerge.nonEmpty

    } while (stillDirty)
  }

  def headAnd [T] (l:List[T]):(Option[T], List[T]) = (l.headOption, l.tail)
  def unsafeHeadAnd [T] (l:List[T]):(T, List[T]) = (l.head, l.tail)

  def collect(ptree: ParseTree, ltree:LoopingTree, depth:Int, states:Set[State], stateMap: Map[Terminal, State]):Unit = {
    val collectables = ptree.getDepth(depth) ++ ptree.getDepth(depth - 1)

    collectables
      .foreach { pLeaf => {
        val maybeLLeaf = ltree.navigateToTerminal(pLeaf.observed.toString, states.flatMap(_.terminals).toSet)
        val maybeState = maybeLLeaf.flatMap { terminal => stateMap.get(terminal) }

        maybeState.foreach { state => state.addHistory(pLeaf) }
      } }
  }
}

