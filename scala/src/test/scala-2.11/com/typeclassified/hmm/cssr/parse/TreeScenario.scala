package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.ProbablisticAsserts
import org.scalatest.{WordSpec, BeforeAndAfter, Matchers, FlatSpec}

class TreeScenario extends WordSpec with Matchers with ProbablisticAsserts with LeafAsserts with BeforeAndAfter {
  AlphabetHolder.alphabet = Alphabet("01".toCharArray)
  var tree:Tree = Tree.loadData("110111011101110111011101110111011101".toArray, 5)

  "loading the data" when {
    val node_____r = tree.root

    val layer1     = node_____r.children
    val node____0r = layer1.find(_.observed == "0")
    val node____1r = layer1.find(_.observed == "1")

    "examining the root layer" should {
      "have correct frequency and distributions" in assertProbabalisticDetails(node_____r, 36, Array(9, 27))
      "have expected leaf properties" in assertChildrenByExactBatch(layer1, Array("0", "1"))
    }

    "examining the 1st layer" should {
      "have the correct children" in {
        node____0r should not be empty
        node____1r should not be empty
        val expected = Array(node____0r.get, node____1r.get)

        layer1 should have size expected.length
        layer1 should contain theSameElementsAs expected
      }
      "have each have the correct frequency and distributions" in {
        assertProbabalisticDetails(node____0r.get, 36, Array(9, 27))
        assertProbabalisticDetails(node____1r.get, 36, Array(9, 27))
      }
      "have expected leaf properties" in {
        assertChildrenByExactBatch(node____0r.get.children, Array("10"))
        assertChildrenByExactBatch(node____1r.get.children, Array("11","01"))
      }
    }
    val layer2     = layer1.flatMap(_.children)
    val node___10r = layer2.find(_.observed == "10")
    val node___11r = layer2.find(_.observed == "11")
    val node___01r = layer2.find(_.observed == "01")

    "examining the 2nd layer" should {
      "have the correct children" in {
        node___10r should not be empty
        node___11r should not be empty
        node___01r should not be empty
        val expected = Array(node___10r.get, node___11r.get, node___01r.get)

        layer2 should have size expected.length
        layer2 should contain theSameElementsAs expected
      }
      "have each have the correct frequency and distributions" in {
        assertProbabalisticDetails(node___10r.get, 36, Array(9, 27))
        assertProbabalisticDetails(node___11r.get, 36, Array(9, 27))
        assertProbabalisticDetails(node___01r.get, 36, Array(9, 27))
      }
      "have expected leaf properties" in {
        assertChildrenByExactBatch(node___10r.get.children, Array("110"))
        assertChildrenByExactBatch(node___11r.get.children, Array("011","111"))
        assertChildrenByExactBatch(node___01r.get.children, Array("101"))
      }
    }

    val layer3     = layer2.flatMap(_.children)
    val node__110r = layer3.find(_.observed == "110")
    val node__011r = layer3.find(_.observed == "011")
    val node__111r = layer3.find(_.observed == "111")
    val node__101r = layer3.find(_.observed == "101")

    val layer4     = layer3.flatMap(_.children)
    val node_1110r = layer4.find(_.observed == "1110")
    val node_1011r = layer4.find(_.observed == "1011")
    val node_0111r = layer4.find(_.observed == "0111")
    val node_1111r = layer4.find(_.observed == "1111")
    val node_1101r = layer4.find(_.observed == "1101")

    val layer5     = layer4.flatMap(_.children)
    val node01110r = layer5.find(_.observed == "01110")
    val node11110r = layer5.find(_.observed == "11110")
    val node11011r = layer5.find(_.observed == "11011")
    val node10111r = layer5.find(_.observed == "10111")
    val node01111r = layer5.find(_.observed == "01111")
    val node11111r = layer5.find(_.observed == "11111")
    val node11101r = layer5.find(_.observed == "11101")

  }
}
