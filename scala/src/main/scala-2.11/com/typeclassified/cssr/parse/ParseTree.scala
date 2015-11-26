package com.typeclassified.cssr.parse

import com.typeclassified.cssr.{CSSR, Probablistic, State}

object ParseTree {
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

class ParseTree {
  var root:List[ParseNode] = List()

  def updatePredictiveDistribution(x0:Char, x_hist:List[Char]) = {
    // navigate from root to x_hist-leaf and update the distribution with x0
    navigateHistory(x_hist).updateDistribution(x0)
  }

  def navigateHistory(history:List[Char]) : ParseNode = {
    // TODO
    return new ParseNode("arst", this, CSSR.emptyState)
  }
}

class ParseNode (string:String, parseTree: ParseTree, initializingState:State) extends Probablistic {
  /* history = 00
   * next_x  = 1
   *       ==> 001
   */
  val history:String = string
  var currentState:State = initializingState
  var children:List[ParseNode] = List()

  def updateDistribution(xNext:Char) = {
    val idx:Int = AlphabetHolder.alphabet.map(xNext)
    frequency(idx) += 1
    totalCounts += 1
    normalDistribution = frequency :/ totalCounts.toDouble
  }

  def changeState(s:State):Unit = {
    // s.append(this) # see null hypothesis and uncomment one
    currentState = s
    // we ought to update transitions here (but for phase II it's not terribly important)
    children foreach( child => child.changeState(s) )
  }
}
