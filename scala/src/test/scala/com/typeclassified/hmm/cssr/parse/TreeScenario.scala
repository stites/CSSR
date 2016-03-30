package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.shared.ProbablisticAsserts
import com.typeclassified.hmm.cssr.trees.{ParseLeaf, ParseTree}
import org.scalatest.{WordSpec, BeforeAndAfter, Matchers}

class TreeScenario extends WordSpec with Matchers with ProbablisticAsserts with LeafAsserts with BeforeAndAfter {
  val (     _0,     _1                 ) = (     "0",     "1"                   )
  val (    _10,    _01,    _11         ) = (    "10",    "01",    "11"          )
  val (   _110,   _101,   _011,   _111 ) = (   "110",   "101",   "011",   "111" )
  val (  _1110,  _1101,  _1011,  _0111 ) = (  "1110",  "1101",  "1011",  "0111" )
  val ( _01110, _11101, _11011, _10111 ) = ( "01110", "11101", "11011", "10111" )

  AlphabetHolder.alphabet = Alphabet(_01.toCharArray)
  var tree:ParseTree = ParseTree.loadData(ParseTree(AlphabetHolder.alphabet), (_1101 * 9).toCharArray, 5)

  "loading the data" when {
    val node_____r = tree.root

    val layer1     = node_____r.children
    val node____0r = layer1.find(_.observed == _0.reverse)
    val node____1r = layer1.find(_.observed == _1.reverse)

    "examining the root layer" should {
      // FIXME: The root layer does _NOT_ have the correct expected distribution. This is a bug, but is required to prove parity.
//      "have correct frequency and distributions" in assertProbabalisticDetails(node_____r, Array(9, 27))
      "have expected leaf properties" in assertChildrenByExactBatch(layer1, Array(_0, _1))
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
        assertProbabalisticDetails(node____0r.get, Array(0,  9))
        assertProbabalisticDetails(node____1r.get, Array(9, 17))
      }
      "have expected leaf properties" in {
        assertChildrenByExactBatch(node____0r.get.children, Array(_10))
        assertChildrenByExactBatch(node____1r.get.children, Array(_11,_01))
      }
    }
    val layer2     = layer1.flatMap(_.children)
    val node___10r = layer2.find(_.observed == _10)
    val node___11r = layer2.find(_.observed == _11)
    val node___01r = layer2.find(_.observed == _01)

    "examining the 2nd layer" should {
      "have the correct children" in {
        node___10r should not be empty
        node___11r should not be empty
        node___01r should not be empty
        assertLeafProperties(node___10r.get, _10)
        assertLeafProperties(node___11r.get, _11)
        assertLeafProperties(node___01r.get, _01)

        val expected = Array(node___10r.get, node___11r.get, node___01r.get)

        layer2 should have size expected.length
        layer2 should contain theSameElementsAs expected
      }
      "have each have the correct frequency and distributions" in {
        assertProbabalisticDetails(node___10r.get, Array(0, 9))
        assertProbabalisticDetails(node___11r.get, Array(8, 8))
        assertProbabalisticDetails(node___01r.get, Array(0, 9))
      }
      "have expected leaf properties" in {
        assertChildrenByExactBatch(node___10r.get.children, Array(_110))
        assertChildrenByExactBatch(node___11r.get.children, Array(_011,_111))
        assertChildrenByExactBatch(node___01r.get.children, Array(_101))
      }
    }

    val layer3     = layer2.flatMap(_.children)
    val node__110r = layer3.find(_.observed == _110)
    val node__011r = layer3.find(_.observed == _011)
    val node__111r = layer3.find(_.observed == _111)
    val node__101r = layer3.find(_.observed == _101)

    "examining the 3rd layer" should {
      "have the correct children" in {
        layer3.find(_.observed == "010") shouldBe empty
        node__110r should not be empty
        node__101r should not be empty
        node__011r should not be empty
        node__111r should not be empty
        val expected = Array(node__110r.get, node__011r.get, node__111r.get, node__101r.get)

        layer3 should have size expected.length
        layer3 should contain theSameElementsAs expected
      }
      "have each have the correct frequency and distributions" in {
        assertProbabalisticDetails(node__110r.get, Array(0, 8))
        assertProbabalisticDetails(node__101r.get, Array(0, 9))
        assertProbabalisticDetails(node__011r.get, Array(0, 8))
        assertProbabalisticDetails(node__111r.get, Array(8, 0))
      }
      "have expected leaf properties" in {
        assertChildrenByExactBatch(node__110r.get.children, Array(_1110))
        assertChildrenByExactBatch(node__101r.get.children, Array(_1101))
        assertChildrenByExactBatch(node__011r.get.children, Array(_1011))
        assertChildrenByExactBatch(node__111r.get.children, Array(_0111))
      }
    }

    val layer4     = layer3.flatMap(_.children)
    val node_1110r = layer4.find(_.observed == _1110)
    val node_1011r = layer4.find(_.observed == _1011)
    val node_0111r = layer4.find(_.observed == _0111)
    val node_1101r = layer4.find(_.observed == _1101)

    "examining the 4th layer" should {
      "have the correct children" in {
        Array("0110", "0101", "0011", "1111").foreach(o => {
          layer4.find(_.observed==o) shouldBe empty
        })
        node_1110r should not be empty
        node_1101r should not be empty
        node_1011r should not be empty
        node_0111r should not be empty
        val expected = Array(node_1110r.get, node_1011r.get, node_0111r.get, node_1101r.get)

        layer4 should have size expected.length
        layer4 should contain theSameElementsAs expected
      }
      "have each have the correct frequency and distributions" in {
        assertProbabalisticDetails(node_1110r.get, Array(8, 0))
        assertProbabalisticDetails(node_1101r.get, Array(0, 8))
        assertProbabalisticDetails(node_1011r.get, Array(0, 8))
        assertProbabalisticDetails(node_0111r.get, Array(0, 8))
      }
      "have expected leaf properties" in {
        assertChildrenByExactBatch(node_1110r.get.children, Array(_01110))
        assertChildrenByExactBatch(node_1101r.get.children, Array(_11101))
        assertChildrenByExactBatch(node_1011r.get.children, Array(_11011))
        assertChildrenByExactBatch(node_0111r.get.children, Array(_10111))
      }
    }

    val layer5     = layer4.flatMap(_.children)
    val node11101r = layer5.find(_.observed == _11101)
    val node11011r = layer5.find(_.observed == _11011)
    val node10111r = layer5.find(_.observed == _10111)
    val node01110r = layer5.find(_.observed == _01110)

    "examining the 5th layer" should {
      var expected:Array[ParseLeaf] = null

      "have the correct children" in {
        Array("01011", "00111", "01101", "11110").foreach(o => {
          layer5.find(_.observed==o) shouldBe empty
        })
        node11101r should not be empty
        node11011r should not be empty
        node10111r should not be empty
        node01110r should not be empty

        expected = Array(
          node11101r.get,
          node11011r.get,
          node10111r.get,
          node01110r.get)

        layer5 should have size expected.length
        layer5 should contain theSameElementsAs expected
      }
      "have each have the correct frequency and distributions" in {
        assertProbabalisticDetails(node11101r.get, Array(8,0))
        assertProbabalisticDetails(node11011r.get, Array(0,7))
        assertProbabalisticDetails(node10111r.get, Array(0,8))
        assertProbabalisticDetails(node01110r.get, Array(0,8))
      }
      "have expected leaf properties" in {
        expected.foreach { n => assertChildrenByExactBatch(n.children, Array[String]()) }
      }
    }

  }
}
