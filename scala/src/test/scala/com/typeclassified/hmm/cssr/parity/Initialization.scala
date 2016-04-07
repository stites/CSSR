package com.typeclassified.hmm.cssr.parity

import com.typeclassified.hmm.cssr.CSSR
import com.typeclassified.hmm.cssr.cli.Config
import com.typeclassified.hmm.cssr.parse.LeafAsserts
import com.typeclassified.hmm.cssr.shared.{FileHandlers, ProbablisticAsserts}
import com.typeclassified.hmm.cssr.trees.ParseTree
import org.scalatest.{WordSpec, Matchers}

class Initialization extends WordSpec with Matchers with ProbablisticAsserts with LeafAsserts with FileHandlers {

  "the even process" when {
    val data = dataFile("even-process", "EP")
    val config = new Config(binaryAlphabet, data, 4, 0.001)

    "initializing CSSR" should {
      val tree: ParseTree = CSSR.initialization(config)

      "have a root with distribution ~= [1/3, 2/3]" in {
        assertApproximateDistribution(tree.root.distribution, Array(1 / 3d, 2 / 3d))
      }

      "have parse leaves 0 and 1 under \0" in {
        assertChildrenByExactBatch(tree.root.children, List("0", "1"))
        val node0 = tree.root.children.filter(_.observation == '0')
        assertApproximateDistribution(node0.head.distribution, Array(1 / 2d, 1 / 2d))
        val node1 = tree.root.children.filter(_.observation == '1')
        assertApproximateDistribution(node1.head.distribution, Array(1 / 4d, 3 / 4d))
      }

      "have parse leaves 10 and 00 under 0" in {
        val node0 = tree.root.children.filter(_.observation == '0').head
        assertChildrenByExactBatch(node0, List("10", "00"))

        val node00 = node0.children.filter(_.observation == '0').head
        val node10 = node0.children.filter(_.observation == '1').head

        assertApproximateDistribution(node00.distribution, Array(1 / 2d, 1 / 2d))
        assertApproximateDistribution(node10.distribution, Array(1 / 2d, 1 / 2d))
      }

      "have parse leaves 11 and 01 under 1" in {
        val node1 = tree.root.children.filter(_.observation == '1').head

        assertChildrenByExactBatch(node1, List("11", "01"))
        val node01 = node1.children.filter(_.observation == '0').head
        val node11 = node1.children.filter(_.observation == '1').head

        assertApproximateDistribution(node01.distribution, Array(0d, 1d))
        assertApproximateDistribution(node11.distribution, Array(1 / 3d, 2 / 3d))
      }

      "parse leaf 01" should {
        "have the distribution [0., 1.]" in {
          val node01 = tree.root
            .children.filter(_.observation == '1').head
            .children.filter(_.observation == '0').head

          // we're piggy-backing the validation of navigate History tests for the moment
          val found01 = tree.navigateHistory("01")
          found01 should not be empty
          found01.get should be(node01)

          assertApproximateDistribution(found01.get.distribution, Array(0d, 1d))
        }

        "have a distribution that reflects we can navigate to 011" in {
          tree.navigateHistory("011") should not be (empty)
        }

        "have a distribution that reflects we cannot navigate to 010" in {
          tree.navigateHistory("010") should be(empty)
        }

        "have children of 001 and 101" in {
          val found01 = tree.navigateHistory("01")
          assertChildrenByExactBatch(found01.get, List("001", "101"))
        }
      }
    }
  }
}
