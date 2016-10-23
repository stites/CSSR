package com.typeclassified.hmm.cssr.trees

import breeze.linalg.DenseVector
import com.typeclassified.hmm.cssr.CSSR.Terminal
import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, Alphabet}
import com.typeclassified.hmm.cssr.shared.EmpiricalDistribution

import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object LoopingTree {
  def allHomogeneous(tree: ParseTree, w:ParseLeaf): Boolean = homogeneous(allPrefixes(tree, w), w)
  def nextHomogeneous(tree: ParseTree)(w:ParseLeaf): Boolean = homogeneous(nextPrefixes(tree, w), w)

  def homogeneous(allHistories:ListBuffer[ParseLeaf], w:ParseLeaf): Boolean = allHistories.forall { pw => Tree.matches(w)(pw) }

  def allExcisable(tree:ParseTree)(w:ParseLeaf, e:String):Boolean = excisable(tree, allPrefixes(tree, w))(w, e)
  def nextExcisable(tree:ParseTree)(w:ParseLeaf, e:String):Boolean = excisable(tree, nextPrefixes(tree, w))(w, e)

  def excisable(tree:ParseTree, uews: ListBuffer[ParseLeaf])(w:ParseLeaf, e:String):Boolean = {
    excise(tree, uews)(w, e)
      .forall {
        case (uew, Some(uw)) => Tree.matches(uew)(uw)
        case (uew, None)     => true
        case _               => true
      }
  }

  def excise(tree:ParseTree, uews: ListBuffer[ParseLeaf])(w:ParseLeaf, e:String):ListBuffer[(ParseLeaf, Option[ParseLeaf])] = {
    val exciseStr:(String)=>String = (uew) => uew.take(uew.length - e.length - w.observed.length) + w

    uews
      .map { uew => (uew, exciseStr(uew.observed)) }
      .map { case (uew, uwStr) => (uew, tree.navigateHistoryRev(uwStr)) }
  }

  /**
    * A more "pure" form of excising: given some history {@code uew} with excisable {@code uew}, return {@code uw}.
    */
  protected def excise(w:String, e:String)(uew: String):String = uew.take(uew.length - e.length - w.length) + w

  /**
    * Get all prefixed histories from the parsetree for a given depth. For example, given the history "1" and a prefix
    * depth of 2, we get leaves: 1, 01, 11.
    */
  def prefixes(tree: ParseTree, w:String, prefixDepth:Int):ListBuffer[ParseLeaf] = {
    val histories = (w.length to prefixDepth)
      .flatMap { n => tree.getDepth(n) }
      .to[ListBuffer]

    histories.filter { _.observed.takeRight(w.length) == w }
  }

  def nextPrefixes(tree: ParseTree, w:ParseLeaf):ListBuffer[ParseLeaf] = nextPrefixes(tree, w.observed)
  def nextPrefixes(tree: ParseTree, w:String):ListBuffer[ParseLeaf] = prefixes(tree, w, w.length+1)

  def allPrefixes(tree: ParseTree, w:ParseLeaf):ListBuffer[ParseLeaf] = allPrefixes(tree, w.observed)
  def allPrefixes(tree: ParseTree, w:String):ListBuffer[ParseLeaf] = prefixes(tree, w, tree.maxLength)

  def leafChildren(lleaves:Iterable[(Char, Node)]):Iterable[LLeaf] = lleaves.toMap.values.foldLeft(List[LLeaf]()){
    case (list, Left(a)) => list :+ a
    case (list, Right(a)) => a match {
      case Left(_) => list
      case Right(e) => list :+ e.value
    }
  }

  def lleafChildren(lleaves:Iterable[(Char, Node)]):Iterable[LLeaf] = lleaves.toMap.values.foldLeft(List[LLeaf]()){
    case (list, Left(a)) => list :+ a
    case (list, Right(Left(l))) => list :+ l
    case (list, Right(Right(e))) => list :+ e.value
  }

  // FIXME: Make all looping nodes data LLeaf = LLeaf { histories::[String] {-which will handle loops-}, children::[LLeaf], isEdge :: Boolean }
  type AltNode = Either[Loop, EdgeSet]
  type Node = Either[LLeaf, AltNode]

  def findLoop(lleaf:LLeaf):Option[Loop] = Tree.firstExcisable(lleaf).flatMap( l => Some(new Loop(l, lleaf)) )

  def findEdgeSet(ltree: LoopingTree, lleaf: LLeaf):Option[EdgeSet] = {
    val terminals = ltree.terminals -- Tree.getAncestorsRecursive(lleaf).toSet[LLeaf]
    val terminalMatches:Set[LLeaf] = terminals.filter(Tree.matches(lleaf))
    if (terminalMatches.isEmpty) None else {
      lleaf.isEdge = true
      terminalMatches.foreach(_.isEdge = true)
      Option.apply(new EdgeSet(lleaf, terminalMatches))
    }
  }

  def findAlt(loopingTree: LoopingTree)(lleaf: LLeaf):Option[AltNode] = {
    lazy val edgeSet = findEdgeSet(loopingTree, lleaf)

    LoopingTree.findLoop(lleaf) match {
      case Some(l) => Some(Left(l))
      case None => if (edgeSet.isEmpty) None else Some(Right(edgeSet.get))
    }
  }

  def getLeaf(node:LoopingTree.Node):LLeaf = node match {
    case Left(lnode) => lnode
    case Right(Right(es)) => es.value
    case Right(Left(loop)) => loop.value
  }
}

class LoopingTree(val alphabet:Alphabet, root:LLeaf) extends Tree[LLeaf](root) {
  /** the set of non-looping terminal nodes */
  var terminals:Set[LLeaf] = HashSet()

  def this(ptree: ParseTree) = {
    this(ptree.alphabet, new LLeaf(ptree.root.observed, Set(ptree.root)))
    terminals = terminals + root
  }

  def navigateLoopingTree(history:String):Option[LLeaf] = {
    def go(history:String, active:Option[LoopingTree.Node], terminals: Option[Set[Terminal]]):Option[LLeaf] = {
      val current: String => Char = _.last
      val prior: String => String = _.init
      val activeLeaf = active.flatMap { node => Option(LoopingTree.getLeaf(node)) }
      val nextNode = activeLeaf.flatMap { leaf => if (history.isEmpty) None else leaf.nextLeaf(current(history)) }

      if (nextNode.exists{ isTerminal(terminals) }) {
        nextNode.map { LoopingTree.getLeaf }
      } else if (nextNode.isEmpty) {
        active.flatMap {
          case Right(Left(node)) => Option(node)
          case Right(Right(edgeSet)) => Option(edgeSet.value)
          case node => Option(LoopingTree.getLeaf(node))
        }

      } else {
        go(prior(history), nextNode, terminals)
      }
    }
    go(history, Some(Left(root)), Option(terminals))
  }

  def navigateToLLeafButStopAtLoops(history: String, terminals:Set[Terminal]):Option[LLeaf] = {
    //print("navigating to LLeaf but stopping at loops and terms: " + history)
    navigateToLLeafButStopAtLoops(history, Some(Left(root)), _.last, _.init, Option(terminals))
  }

  def navigateToLLeafButStopAtLoops(history: String):Option[LLeaf] = {
    //print(s"navigating to LLeaf but stopping at loops: " + history)
    navigateToLLeafButStopAtLoops(history, Some(Left(root)), _.last, _.init, None)
  }

  def isTerminal(terminals:Option[Set[Terminal]])(node:LoopingTree.Node):Boolean = {
    if (terminals.isEmpty) false else {
      val fromTerminals = isTerminal(terminals.get)(_)

      node match {
        case Left(lleaf) => fromTerminals(lleaf)
        case Right(Left(loop)) => fromTerminals(loop)
        case Right(Right(edgeSet)) => fromTerminals(edgeSet.value)
      }
    }
  }

  def isTerminal(terminals:Set[Terminal])(lleaf:LLeaf):Boolean = terminals.nonEmpty && terminals.contains(lleaf)

  def navigateToLLeafButStopAtLoops(history: String, active:Option[LoopingTree.Node], current:(String)=>Char, prior:(String)=>String, terminals:Option[Set[Terminal]]): Option[LLeaf] = {
    val isTerminalPredicate = isTerminal(terminals)(_)
    val nextLeaf = active.flatMap { node => Option(LoopingTree.getLeaf(node)) }
    val nextNode = nextLeaf.flatMap { leaf => if (history.isEmpty) None else leaf.nextLeaf(current(history)) }
    val nextNodeIsTerminal = nextNode.exists{ isTerminalPredicate }

    if (nextNode.isEmpty || nextNodeIsTerminal) {
      val possibleloopOrLeaf = active.flatMap {
        case Right(Left(node)) => Option(node)
        case Right(Right(edgeSet)) => {
          Option(edgeSet.value)
        }
        case node => Option(LoopingTree.getLeaf(node))
      }

      val result:Option[LLeaf] = if (nextNodeIsTerminal) nextNode.map{LoopingTree.getLeaf} else possibleloopOrLeaf

      //println(": " + result.toString)
      result
    } else {
      navigateToLLeafButStopAtLoops(prior(history), nextNode, current, prior, terminals)
    }
  }

  def navigateToTerminal(history: String, terminals:Set[Terminal]): Option[LLeaf] = {
    //print("navigating TERMINAL: " + history)
    navigateToTerminal(history, Some(Left(root)), _.last, _.init, terminals)
  }

  def navigateToTerminal(history: String, active:Option[LoopingTree.Node], current:(String)=>Char, prior:(String)=>String, terminals:Set[Terminal]): Option[LLeaf] = {

    val maybeLLeaf = navigateToLLeafButStopAtLoops(history, active, current, prior, Some(terminals))

    maybeLLeaf.flatMap {
      lastLeaf => {
        if (terminals.contains(lastLeaf)) {
          Option(lastLeaf)
        } else {
          // Option(lastLeaf.terminalReference.getOrElse(None /* perhaps this should be lastLeaf */))
          lastLeaf.terminalReference
        }
      }
    }
  }

  def collectLeaves(loops:Iterable[Loop]):Array[LLeaf] = loops.toArray ++ collectLeaves(loops.map(_.value))
}

// a simple abstract marker class for a looping tree's Loops and Edges
abstract class LoopWrapper(val value:LLeaf) {
  override def toString: String = getClass.getSimpleName + "(" + value.toString() + ")"
}

class Loop(val value: LLeaf, observed:String, seededHistories:Set[ParseLeaf] = Set(), parent:Option[LLeaf] = None) extends LLeaf(observed, seededHistories, parent) {
  def this(value:LLeaf, origin:LLeaf) = this(value, origin.observed, origin.histories, origin.parent)

  var asTerminal:Option[LLeaf] = None

  override def toString: String = s"""Loop{$observed, Loop(${value.toString})}"""
}

class EdgeSet (edge: LLeaf, val edges:Set[LLeaf]) extends LoopWrapper(edge) {
  edge.isEdge = true
}

class LLeaf(val observed:String, seededHistories:Set[ParseLeaf] = Set(), parent:Option[LLeaf] = None) extends Leaf[LLeaf] (if ("".equals(observed)) 0.toChar else observed.head, parent) with EmpiricalDistribution {
  histories = seededHistories
  recalculateHists(histories)

  var children:mutable.Map[Char, LoopingTree.Node] = mutable.Map()

  var edgeSet:Option[mutable.Set[LLeaf]] = None

  var isEdge: Boolean = false

  def this(p: ParseLeaf) = this(p.observed, Set(p), None)

  override def getChildren():Iterable[LLeaf] = LoopingTree.lleafChildren(children)

  def nextLeaf(c: Char): Option[LoopingTree.Node] = children.get(c)

  // this pretty much duplicates LoopingTree.getLeaf
  override def next(c: Char): Option[LLeaf] = children.get(c).flatMap { node => Some(LoopingTree.getLeaf(node)) }

  override def toString:String = {
    val rDist = rounded.toArray.mkString("[", ",", "]")
    val nChildren = children.keys.size
    s"{$observed, rounded:$rDist, size: ${histories.size}, children: $nChildren, isEdge: $isEdge}"
  }

  // temp debugging purposes
  var originalDistribution:DenseVector[Double] = DenseVector.zeros(AlphabetHolder.alphabet.length)
  var terminalReference:Option[LLeaf] = None

  def refineWith(lLeaf: LLeaf):Unit = {
    originalDistribution = distribution
    distribution = lLeaf.distribution
    terminalReference = Option(lLeaf)
  }
}

