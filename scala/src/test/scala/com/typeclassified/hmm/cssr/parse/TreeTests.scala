package com.typeclassified.hmm.cssr.parse

import breeze.linalg
import breeze.linalg.DenseVector
import com.typeclassified.hmm.cssr.shared.ProbablisticAsserts
import org.scalatest.{WordSpec, BeforeAndAfter, Matchers, FlatSpec}

import scala.collection.mutable.ListBuffer

class TreeTests extends WordSpec with Matchers with ProbablisticAsserts with LeafAsserts with BeforeAndAfter {
  var tree:Tree = null
  val abc = "abc"
  val abcabc = "abcabc"
  val abcbabcbb = "abcbabcbb"
  val testMap = Map(
    abc -> Array(
      Array("a", "b", "c"),
      Array("ab", "bc"),
      Array("abc")
    ),
    abcabc -> Array(
      Array("a", "b", "c"),
      Array("ab", "bc", "ca"),
      Array("abc", "bca", "cab")
    ),
    abcbabcbb -> Array(
      Array("a", "b", "c"),
      Array("ab", "bc", "cb", "ba", "bb"),
      Array("abc", "bcb", "cba", "bab", "cbb")
    )
  )

  before {
    AlphabetHolder.alphabet = Alphabet(abc.toCharArray)
    tree = Tree(AlphabetHolder.alphabet)
  }

  "loadHistory" should {

    "generating a single branch for an empty tree" in {
      var children:ListBuffer[Leaf] = null
      var leaf:Leaf = null
      Tree.loadHistory(tree, abc)

      children = tree.root.children
      children should have size 1

      leaf = children.head
      assertLeafProperties(leaf, "c")

      children = children.head.children
      children should have size 1

      leaf = children.head
      assertLeafProperties(leaf, "bc")

      children = children.head.children
      children should have size 1

      leaf = children.head
      assertLeafProperties(leaf, abc)

      leaf.observed    should equal (abc)
      leaf.observation should equal (abc.head)
    }

    "generating a long single branch for an empty tree" in {
      Tree.loadHistory(tree, "abcabc")
      var leaf:Leaf = tree.root

      while (leaf.children.nonEmpty) {
        leaf = leaf.children.head
      }

      assertLeafProperties(leaf, "abcabc")
    }

    "generating multiple root branches if they do not exist" in {
      Tree.loadHistory(tree, "bc")
      Tree.loadHistory(tree, "aa")

      val children = tree.root.children
      children should have size 2

      val leaf1 = children(0)
      assertLeafProperties(leaf1, "c")
      leaf1.children should have size 1
      val leaf1Child = leaf1.children.head
      assertLeafProperties(leaf1Child, "bc")


      val leaf2 = children(1)
      assertLeafProperties(leaf2, "a")
      leaf2.children should have size 1
      val leaf2Child = leaf2.children.head
      assertLeafProperties(leaf2Child, "aa")

      Tree.loadHistory(tree, "ba")

      children should have size 2
      leaf1.children should have size 1
      leaf2.children should have size 2
      leaf2Child.children should have size 0
      leaf1Child.children should have size 0

      val leaf2NewChild = leaf2.children.filter(_ != leaf2Child).head
      assertLeafProperties(leaf2NewChild, "ba")
    }

    "updating the distribution of the path to the terminal leaf, if the path does not exist" in {
      Tree.loadHistory(tree, "cb") // loads "cb", updates
      val root = tree.root
      var leafInQuestion = root
      assertProbabalisticDetails(leafInQuestion, Array(0,1,0))

      val rootB = leafInQuestion.children.head
      leafInQuestion = rootB
      assertProbabalisticDetails(leafInQuestion, Array(0,0,1))

      val rootBC = leafInQuestion.children.head
      leafInQuestion = rootBC
      assertProbabalisticDetails(leafInQuestion, Array(0,0,0))
    }

    "updating the distribution of a leaf for it's next-step" should {
      "update the path to the terminal leaf" in {
        Tree.loadHistory(tree, "cb")
        val root = tree.root
        val rootB = root.children.head
        val rootBC = rootB.children.head

        assertProbabalisticDetails(root, Array(0, 1, 0))

        assertProbabalisticDetails(rootB, Array(0, 0, 1))

        assertProbabalisticDetails(rootBC, Array(0, 0, 0))
      }

      "ignore path if already exists" in {
        Tree.loadHistory(tree, "cb")
        val root = tree.root
        val rootB = root.children.head
        val rootBC = rootB.children.head

        Tree.loadHistory(tree, "ab")
        assertProbabalisticDetails(root, Array(0, 1, 0))
        assertProbabalisticDetails(rootB, Array(1, 0, 1))
        assertProbabalisticDetails(rootB.children.head, Array(0, 0, 0))

        Tree.loadHistory(tree, "a")
        assertProbabalisticDetails(root, Array(1, 1, 0))

        assertProbabalisticDetails(root.children.last, Array(0, 0, 0))
      }
    }
  }

  "loadData" when {
    "loading sequence 'abc' at lMax of 3 (windows of: abc, ab, bc, a, b, c)" should {

      "load all complete moving windows" in {
        tree = Tree.loadData(tree, abc.toArray, 3)

        var children:ListBuffer[Leaf] = tree.root.children
        assertChildrenByExactBatch(children, testMap(abc)(0))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abc)(1))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abc)(2))

        children = children.flatMap(_.children)
        children should have size 0
      }
    }
    "loading sequence 'abcabc' at lMax of 3 (windows of:  abc, bca, cab, ab, bc, ca, a, b, c)" should {
      "load all combinations" in {
        tree = Tree.loadData(tree, abcabc.toArray, 3)

        var children:ListBuffer[Leaf] = tree.root.children
        assertChildrenByExactBatch(children, testMap(abcabc)(0))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abcabc)(1))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abcabc)(2))

      }

      "include lMax+1 probabilities but not lMax+1 children" in {
        tree = Tree.loadData(tree, abcabc.toArray, 3)
        var children:ListBuffer[Leaf] = tree.root.children // l1
        children = children.flatMap(_.children) //l2
        children = children.flatMap(_.children) //l3
        children = children.flatMap(_.children) //l4
        children should have size 0
      }
    }

    "loading sequence 'abcbabcbb' at lMax of 3 (windows of: abc, bcb, cba, bab, cbb, ab, bc, cb, ba, bb, a, b, c)" should {
      "load all combinations" in {
        tree = Tree.loadData(tree, abcbabcbb.toArray, 3)

        var children:ListBuffer[Leaf] = tree.root.children
        assertChildrenByExactBatch(children, testMap(abcbabcbb)(0))

        assertChildrenByExactBatch(children.filter(_.observation == 'b').head.children, Seq("ab", "cb", "bb"))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abcbabcbb)(1))

        assertChildrenByExactBatch(children.filter(_.observation == 'b').flatMap(_.children), Seq("abc", "cba", "cbb"))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abcbabcbb)(2))

      }
      "finished, it should also include lMax+1 probabilities but not lMax+1 children" in {
        tree = Tree.loadData(tree, abcbabcbb.toArray, 3)
        var children:ListBuffer[Leaf] = tree.root.children // l1
        children = children.flatMap(_.children) //l2
        children = children.flatMap(_.children) //l3
        children = children.flatMap(_.children) //l4
        children should have size 0
      }
    }

    "not add newlines" in {
      tree = Tree.loadData(tree, (abcabc+"\r\n").toArray, 3)
      assertChildrenByExactBatch(tree.root.children, testMap(abcabc)(0))
    }
  }

  "navigateHistory" should {
    "be able to return the correct leaf" in {
      tree = Tree.loadData(tree, abc.toArray, 3)
      val maybeABC = tree.navigateHistory("abc".toList)
      maybeABC should not be empty
      assertLeafProperties(maybeABC.get, "abc")

      val notPresentA = tree.navigateHistory("abca".toList)
      notPresentA shouldBe empty

      tree = Tree.loadData(tree, abcbabcbb.toArray, 3)

      val maybe_BB = tree.navigateHistory("bb".toList)
      maybe_BB should not be empty
      assertLeafProperties(maybe_BB.get, "bb")

      val maybeCBB = tree.navigateHistory("cbb".toList)
      maybeCBB should not be empty
      assertLeafProperties(maybeCBB.get, "cbb")

      maybe_BB.get.children.map(_.observed) should contain (maybeCBB.get.observed)

      tree = Tree.loadData(tree, "aabbabcab".toArray, 3)

      val maybe_AB = tree.navigateHistory("ab".toList)
      maybe_AB should not be empty
      assertLeafProperties(maybe_AB.get, "ab")

      val maybeAAB = tree.navigateHistory("aab".toList)
      val maybeBAB = tree.navigateHistory("bab".toList)
      val maybeCAB = tree.navigateHistory("cab".toList)

      val maybeChildren = List(maybeAAB, maybeBAB, maybeCAB)

      maybeChildren foreach(_ should not be empty)

      maybe_AB.get.children.map(_.observed) should contain theSameElementsAs maybeChildren.map(_.get.observed)
    }
  }
  "getDepth" should {
    "be able to return the correct leaf" in {
      tree = Tree.loadData(tree, abcabc.toArray, 3)

      tree.getDepth(0).map(_.observed) should contain only ""

      tree.getDepth(1).map(_.observed) should contain theSameElementsAs testMap(abcabc)(0)

      tree.getDepth(2).map(_.observed) should contain theSameElementsAs testMap(abcabc)(1)

      tree.getDepth(3).map(_.observed) should contain theSameElementsAs testMap(abcabc)(2)

      tree.getDepth(4) should have size 0
    }
  }
}
