package com.typeclassified.cssr

import com.typeclassified.cssr.parse.ParseAlphabet
import breeze.linalg._

import scala.collection.mutable.ListBuffer

class CSSR {
  val datasize = 1000
  val A = List('a', 'b')
  var alphabet:ParseAlphabet = ParseAlphabet(A)
  var obs:ListBuffer[Char] = new ListBuffer[Char]()
  val Lmax = 5
  val sig = 0.7

  1 to datasize foreach { i => obs += A(i%2) }

  // ======================================================================================

  class ParseTree {
    var root:List[ParseNode] = List()

    def updatePredictiveDistribution(x0:Char, x_hist:List[Char]) = {
      // navigate from root to x_hist-leaf and update the distribution with x0
      navigateHistory(x_hist).updateDistribution(x0)
    }

    def navigateHistory(history:List[Char]) : ParseNode = {
      // TODO
      return new ParseNode("arst", this)
    }
  }

  object ParseTree {
    var alphabet:ParseAlphabet = ParseAlphabet(A) // <= lols

    def apply() = new ParseTree()

    def loadData(tree:ParseTree, xs:List[Char], n:Int) = {
      //  Yield successive n-sized windows from the x's. Does not work with a length of 0.
      for (size <- 1 until n) {
        for (observed <- xs.iterator.sliding(size).withPartial(false)){
          // updates the predictive distributions of the tree
          tree.updatePredictiveDistribution(observed.head, observed.tail.toList)
        }
      }
    }
  }

  class State (val value:Char) {
    var histories:List[ParseNode] = List()
    val counts:DenseVector[Int] = DenseVector.zeros[Int](histories.size)
    var normalDistribution:DenseVector[Double] = DenseVector.zeros[Double](histories.size)
    var count = 0

    def addHistory (h:ParseNode)= {
      histories += h
      normalize_across_histories()
    }

    def rmHistory (x:ParseNode) = {
      histories = histories.filter( y => y != x)
      normalize_across_histories()
    }

    def normalize_across_histories() = {
      for (node <- histories) {
        count += node.totalCounts
      }

      normalDistribution() =
      normalized_distribution += node.normalized * node.total_count

      normalized_distribution / count
    }

  }
  object State { def apply(c:Char) = new State(c) }

  class ParseNode (string:String, parseTree: ParseTree) {
    /* history = 00
     * next_x  = 1
     *       ==> 001
     */
    var totalCounts:Int = 0
    val history:String = string
    var currentState:State = None
    var children:List[ParseNode] = List()

    val frequency:DenseVector[Int] = DenseVector.zeros[Int](ParseTree.alphabet.size)
    var normalDistribution:DenseVector[Double] = DenseVector.zeros[Double](ParseTree.alphabet.size)

    def updateDistribution(xNext:Char) = {
      val idx:Int = parseTree.alphabet.map(xNext)
      frequency(idx) += 1
      totalCounts += 1
      normalDistribution = frequency :/ totalCounts.toDouble
    }

    """ take 'windows', track current depth in form of history
    take all next "steps" and calculate conditional probabilities
    """

    def changeState(s:State):Unit = {
      // s.append(this) # see null hypothesis and uncomment one
      currentState = s
      // we ought to update transitions here (but for phase II it's not terribly important)
      children foreach( child => child.changeState(s) )
    }
  }

  /*

  }
  class ParseTree {
  var tree:ParseTreeNode = new ParseTreeNode()

  def addString (str:String) :ParseTree = {
    tree = addString(str, new ParseTreeNode())
    this
  }

  protected def addString (str:String, tree:ParseTreeNode): ParseTreeNode = {
    if (str.isEmpty) {
      tree
    } else {
      val nextNode = new ParseTreeNode(str.head)
      tree.children += nextNode
      addString(str.tail, nextNode)
    }
  }

  //  def getDepth (int: Int): List[String] = {
  //    getDepth(int, tree.children.toList, List(tree.value))
  //  }

  /*
  protected def getDepth (int: Int, nodes:List[ParseTreeNode], strings:List[String]):List[String] = {
    if (int <= 0) strings else {
      var nextStrings = Array()
      for (node <- nodes){
        nextStrings ++= strings.map(s => node.value + s)
      }
      val nextNodes = nodes.reduce(List())(acc, node => )
      getDepth(int - 1, nextNodes, nextStrings)
    }
  }
  */
  }

  class ParseTreeNode (val value:Char = 0.toChar) {

  var children:Set[ParseTreeNode] = Set()

  def canEqual(other: Any): Boolean = other.isInstanceOf[ParseTreeNode]

  override def equals(other: Any): Boolean = other match {
    case that: ParseTreeNode =>
      (that canEqual this) &&
        value == that.value
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(value)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
  }
  */
  val allStates:List[State] = List(State(0.toChar))

  // ======================================================================================
}

object CSSR {
  def main(args: Array[String]) = {
    println(new CSSR().obs.toList)
  }
}

