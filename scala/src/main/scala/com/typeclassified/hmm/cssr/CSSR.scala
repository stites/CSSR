package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.shared.Epsilon
import com.typeclassified.hmm.cssr.measure.out.Results
import com.typeclassified.hmm.cssr.shared.{Level, Logging}
import com.typeclassified.hmm.cssr.state.{AllStates, Machine, EquivalenceClass}
import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, Alphabet}
import com.typeclassified.hmm.cssr.trees.LoopingTree.AltNode
import com.typeclassified.hmm.cssr.trees._

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.collection.mutable.ListBuffer

object CSSR extends Logging {
  override def loglevel() = Level.INFO

  // type aliases:
  type State = EquivalenceClass
  type ParentState = EquivalenceClass
  type TransitionState = Option[EquivalenceClass]
  type OrderedHistorySet = mutable.LinkedHashSet[ParseLeaf]
  type States = List[State]
  type MutableStates = ListBuffer[State]

  type HistoryTransitions = Map[ParseLeaf, TransitionState]
  type StateTransitions = Map[Char, HistoryTransitions]
  type AllStateTransitions = Map[ParentState, StateTransitions]
  type StateToStateTransitions = Map[ParentState, Map[Char, TransitionState]]
  type TransitionMemo = Map[String, (ParentState, TransitionState)]

  def run(config: Config):Results  = {
    val (tree: ParseTree, allStates: MutableStates) = initialization(config)
    val ls = tree.collectLeaves()
    implicit val ep:Epsilon = new Epsilon(0.01)

    val looping = grow(tree)

    val transitions = mapTransitions(allStates, tree)
    val finalStates = new AllStates(allStates, transitions)
    val machine = new Machine(finalStates, tree)

    new Results(config, AlphabetHolder.alphabet, tree, machine, finalStates)
      .out(if (config.out) null else config.dataFile)
  }

  def mapTransitions(allStates: MutableStates, pTree: ParseTree):StateToStateTransitions = allStates.map {
    s => s -> s.histories.map {
      h => pTree.alphabet.raw.map {
        c => c -> h.getTransitionState(pTree, allStates, c)
      }.toMap
    }.head
  }.toMap

  def initialization(config: Config): (ParseTree, MutableStates) = {
    val alphabetSrc: BufferedSource = Source.fromFile(config.alphabetFile)
    val alphabetSeq: Array[Char] = try alphabetSrc.mkString.toCharArray finally alphabetSrc.close()

    val dataSrc: BufferedSource = Source.fromFile(config.dataFile)
    val dataSeq: Array[Char] = try dataSrc.mkString.toCharArray finally dataSrc.close()

    val alphabet = Alphabet(alphabetSeq)
    AlphabetHolder.alphabet = alphabet
    val rootClass = EquivalenceClass()
    val parseTree = ParseTree.loadData(ParseTree(alphabet, rootClass), dataSeq, config.lMax)
    rootClass.addHistory(parseTree.root)
    val allStates = ListBuffer(rootClass)

    (parseTree, allStates)
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
            val lleaf:LLeaf = new LLeaf(c, pleaves, Option(active))
            val alternative:Option[LoopingTree.AltNode] = findAlternative(lleaf)
            c -> alternative.toRight(lleaf)
          } }

        active.children ++= nextChildren
        // Now that active has children, it cannot be considered a terminal node. Thus, we elide the active node:
        ltree.terminals = ltree.terminals ++ LoopingTree.leafChildren(nextChildren).toSet[LLeaf] - active
        // FIXME: how do edge-sets handle the removal of an active node?
        activeQueue ++= LoopingTree.leafChildren(nextChildren)
      }
    }

    ltree
  }

  def refine(tree:ParseTree,S:MutableStates,lMax:Double,sig:Double) = {
  }
}

