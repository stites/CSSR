package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.measure.out.Results
import com.typeclassified.hmm.cssr.shared.Epsilon
import com.typeclassified.hmm.cssr.shared.{Level, Logging}
import com.typeclassified.hmm.cssr.state.{AllStates, Machine, State}
import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, Alphabet}
import com.typeclassified.hmm.cssr.trees._

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
    val looping = grow(tree)
    refine(looping)

    val (allStates, transitions, stateMap) = statesAndTransitions(tree, looping)
    // with the looping tree "grown", we now need to collect histories within each state so that we can later examine our distribution
    collect(tree, looping, tree.maxLength, allStates.toSet, stateMap)
    allStates.foreach(_.pruneHistories(tree.maxLength))

    val finalStates = new AllStates(allStates, transitions)
    val machine = new Machine(finalStates, tree)

    new Results(config, AlphabetHolder.alphabet, tree, machine, finalStates, config.stateLabels)
      .out(if (config.out) null else config.dataFile)
  }

  def printParse(parseLeaf: ParseLeaf, nTabs:Int = 0): Unit = {
    println("\t" * nTabs + parseLeaf.toString)
    parseLeaf.children.foreach(this.printParse(_, nTabs+1))
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
    * - generate the root looping node. add to "active" queue
    * - while active queue is not empty:
    *   - remove the first looping node from the queue. With this looping node:
    *     - check to see if it is homogeneous wrt all next-step histories belonging to collection:
    *       - for each h in pnodes, for each c in h.children
    *           if c.distribution ~/= (looping node).distribution, return false for homogeneity.
    *       | if the node makes it through the loop, return true for homogeneity.
    *     | if the looping node is homogeneous
    *       - do nothing, move to next looping node in active queue.
    *     | if the looping node is not homogeneous
    *       - create new looping nodes for all valid children (one for each symbol in alphabet - must have empirical observation in dataset).
    *       - check all new looping nodes for excisability:
    *         - get all ancestors, ordered by depth (root looping node first)
    *         - check to see if distributions are the same (?)
    *         | if the distributions are the same, the node is excisable
    *           - create loop
    *         | if non are the same
    *           - do nothing
    *       - check all new looping nodes for edges:
    *         - get all terminal nodes that are not ancestors
    *         | if there exists terminal nodes with identical distributions
    *           - delete new looping node (under symbol {@code a})
    *           - and add existing terminal node as active node's {@code a} child. This is unidirectional and we call it an "edge"
    *       -  Add all new looping nodes to children of active node, mapped by alphabet symbol
    *       -  Add unexcisable children to queue
    * - end while
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
    * until (no change)
    *  for each terminal node t
    *    for each non-looping path w to t
    *      for each symbol a in the alphabet follow the path wa in the tree
    *        if wa leads to a terminal node
    *          | continue
    *        else (== wa does not lead to a terminal node)
    *          | copy the sub-looping-tree rooted at (the node reached by) wa to t
    *            giving all terminal nodes the predictive distribution of t
    *        break loop
    *
    * Questions:
    *   + what about edge sets?
    *   + if we let a terminal node's distribution override another terminal node's distribution (via subtree) will order matter?
    */

  type wa = String
  type Terminal = LLeaf

  def refine(ltree:LoopingTree) = {
    var stillDirty = false

    do {
      val toCheck: Set[(Terminal, wa)] = ltree.terminals
        .flatMap(tNode => {
          val w = tNode.path().mkString
          ltree.alphabet.raw.map(a => (tNode, w + a))
        } )
        .filter { case (term, wa) => term.distribution(ltree.alphabet.map(wa.last)) > 0 }

      stillDirty = toCheck
        .foldLeft(false){
          case (isDirty:Boolean, (t:Terminal, wa:wa)) => {
            // navigate the looping tree, stopping at terminal nodes
            val foundLeaf = ltree.navigateToLLeafButStopAtLoops(wa)
            // check to see if this leaf is _not_ a terminal leaf
            val nonTerminating = foundLeaf.find{ l => !ltree.terminals.contains(l) }

            // if we find a "terminal-looping" node (ie- any looping node) that is not a terminal node:
            //   | if it loops to a terminal node: merge this node into the terminal node
            //   | else: make its value a terminal node
            // if we find a "terminal-edgeSet" node: merge this node into the terminal node
            // FIXME: currently, we actually skip edgesets and don't count them as an "LLeaf"
            //
            // if either of the above return None, else we have a sub-looping-tree and return Some.
            val foundLoop = nonTerminating
              .find { _.isInstanceOf[Loop] }
              .flatMap {
                case (loop:Loop) => {
                  // if it is not a terminal node: make it a terminal node
                  if (loop.value.terminalReference.isEmpty && !ltree.terminals.contains(loop)) {
                    // FIXME: merge the loop's empirical observations as well - but maybe we should do this above...
                    // ...at any rate, the loop is all we need to prototype this.
                    loop.value.terminalReference = Option(loop)
                    ltree.terminals ++= Set(loop)
                    Option(loop.value)
                  } else if (loop.value.terminalReference.nonEmpty) {
                    // merge this node into the terminal node
                    loop.value.terminalReference.get.addHistories(loop.histories)
                    loop.terminalReference = loop.value.terminalReference
                    None
                  } else {
                    None
                  }
                }
                case _ => None
              }

            // if not, we will paint this sub-tree with the origin t-node distribution
            (if (foundLoop.nonEmpty) foundLoop else nonTerminating)
              .foreach {
                subLeaf => ltree
                  .collectLeaves(ListBuffer(subLeaf))    // ignores edgeSets and loops
                  .filterNot{ ltree.terminals.contains } // terminal nodes cannot be overwritten
                  .foreach{ _.refineWith(t) }
              }

            // if all of the above did work, then a node exists we need to raise this flag
            isDirty || foundLoop.nonEmpty || (nonTerminating.nonEmpty && !nonTerminating.get.isInstanceOf[Loop])
          } }

    } while (stillDirty)
  }

  def collect(ptree: ParseTree, ltree:LoopingTree, depth:Int, states:Set[State], stateMap: Map[Terminal, State]):Unit = {
    val collectables = ptree.getDepth(depth) ++ ptree.getDepth(depth - 1)

    collectables
      .foreach { pLeaf => {
        val maybeLLeaf = ltree.navigateToTerminal(pLeaf.observed.toString, states.flatMap(_.terminals))
        val maybeState = maybeLLeaf.flatMap { terminal => stateMap.get(terminal) }

        maybeState.foreach { state => state.addHistory(pLeaf) }
      } }
  }
}

