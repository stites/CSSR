package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.shared.Epsilon
import com.typeclassified.hmm.cssr.state.{AllStates, Machine, EquivalenceClass}
import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, Alphabet}
import com.typeclassified.hmm.cssr.trees._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.collection.mutable.ListBuffer

object CSSR extends LazyLogging {
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

  def run(config: Config) = {
    val (tree: ParseTree, allStates: MutableStates) = initialization(config)
    val ls = tree.collectLeaves()
    implicit val ep:Epsilon = new Epsilon(0.01)

    val looping = grow(tree)

    recursion(tree, allStates, config.sig, config.lMax)
  }

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
    * Algorithm:
    * - generate the root lnode. add to "active" queue
    * - while active queue is not empty:
    *   - remove the first lnode from the queue. With the lnode:
    *     - check to see if it is homogeneous wrt all next-step histories belonging to collection:
    *       | for each h in pnodes, for each c in h.children
    *       |   if c.distribution ~/= lnode.distribution, return false for homogeneity.
    *       | if the node makes it through the loop, return true for homogeneity.
    *     | if the lnode is homogeneous
    *       - do nothing
    *     | if the lnode is not homogeneous
    *       - create lnodes for all valid children. Add child lnodes to queue
    *       - check all lnodes for excisability:
    *         - get all ancestors, ordered by depth (root lnode first)
    *         - check to see if distributions are the same (?)
    *         | if the distributions are the same, the node is excisable
    *           - create loop (?)
    *         | if non are the same
    *           - do nothing
    * - end while
    *
    * @param tree
    * @return
    */
  def grow(tree:ParseTree):LoopingTree = {
    val ltree = new LoopingTree(tree)
    val activeQueue = ListBuffer[LoopingLeaf](ltree.root)
    while (activeQueue.nonEmpty) {
      val active:LoopingLeaf = activeQueue.remove(0)
      val isHomogeneous:Boolean = active.histories.forall{ pLeaf => LoopingTree.nextHomogeneous(tree, pLeaf) }

      if (isHomogeneous) {
        // do nothing
      } else {
        val nextNodes:Map[Char, LoopingLeaf] = active.histories
          .flatMap{ _.children } // there may be overlap here. for instance [AB, ABA].children => [*ABA*, ABAB] ABA is a problem
          .groupBy{ _.observation }
          .map {
            case (c, pleaves) => {
              // bypass current adding of children for fast iteration
              val lleaf = new LoopingLeaf(c, ltree, pleaves, Option(active))

              // and perform an inspection to validate that this works
              val allMatching = lleaf.histories
                .map{_.distribution}
                .forall { Tree.matches(lleaf.distribution) }

              val outcome = if (allMatching) "great" else "terrible..."
              logger.debug(s"matching went $outcome with ${lleaf.histories.length} histories")

              c -> lleaf
              }
            }

        val nextChildren = nextNodes.map {
          case (c, lleaf) => {
            // create Some(loop) if one exists, None if no loop exists
            lleaf.loop = Tree.firstExcisable(lleaf)
            lleaf
          }
        }

        active.children ++= nextChildren
        activeQueue ++= nextChildren.filter{ _.loop.isEmpty }
      }
    }

    ltree
  }

  def recursion(tree:ParseTree,S:MutableStates,lMax:Double,sig:Double) = {
  }
}

