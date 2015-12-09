package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.ProbablisticAsserts
import org.scalatest.{WordSpec, BeforeAndAfter, Matchers, FlatSpec}

class TreeScenario extends WordSpec with Matchers with ProbablisticAsserts with LeafAsserts with BeforeAndAfter {
  var tree:Tree = null

  before {
    AlphabetHolder.alphabet = Alphabet("01".toCharArray)
    tree = Tree()
    tree = Tree.loadData("110111011101110111011101110111011101".toArray, 5)
  }

  "loading the data" when {
    "examining the root layer" should {
      "have correct frequency and distributions" in assertProbabalisticDetails(tree.root, 36, Array(9, 27))
      "have expected leaf properties" in assertChildrenByExactBatch(tree.root.children, Array("0", "1"))
    }
  }
}
