package com.typeclassified.hmm.cssr.parse

import breeze.linalg
import breeze.linalg.DenseVector
import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

import scala.collection.mutable.ListBuffer

class TreeTests extends FlatSpec with Matchers with BeforeAndAfter {
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

  def assertLeafProperties(leaf:Leaf, obs:String): Unit = {
    leaf.observed should equal (obs)
    leaf.observation should equal (obs.head)
  }

  before {
    AlphabetHolder.alphabet = Alphabet(abc.toArray)
    tree = Tree()
  }

  behavior of "loadHistory"

  it should "generate a single branch for an empty tree" in {
    var children:ListBuffer[Leaf] = null
    var leaf:Leaf = null
    Tree.loadHistory(tree, abc)

    children = tree.root.children
    children should have size 1

    leaf = children.head
    leaf.observed should equal ("c")
    leaf.observation should equal ("c".head)

    children = children.head.children
    children should have size 1

    leaf = children.head
    leaf.observed should equal ("bc")
    leaf.observation should equal ("bc".head)

    children = children.head.children
    children should have size 1

    leaf = children.head
    leaf.observed should equal ("abc")
    leaf.observation should equal ("abc".head)
  }

  it should "generate multiple root branches if they do not exist" in {
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

  it should "update the distribution of a leaf for it's next-step observations" in {
    Tree.loadHistory(tree, "cb")
    val root = tree.root
    var leafInQuestion = root
    leafInQuestion.totalCounts          should be (1)
    leafInQuestion.frequency.toArray    should contain theSameElementsInOrderAs Array[Double](0.0, 1.0, 0.0)
    leafInQuestion.distribution.toArray should contain theSameElementsInOrderAs Array[Double](0.0, 1.0, 0.0)

    val rootB = leafInQuestion.children.head
    leafInQuestion = rootB
    leafInQuestion.totalCounts          should be (1)
    leafInQuestion.frequency.toArray    should contain theSameElementsInOrderAs Array[Double](0.0, 0.0, 1.0)
    leafInQuestion.distribution.toArray should contain theSameElementsInOrderAs Array[Double](0.0, 0.0, 1.0)

    val rootBC = leafInQuestion.children.head
    leafInQuestion = rootBC
    leafInQuestion.totalCounts          should be (0)
    leafInQuestion.frequency.toArray    should contain theSameElementsInOrderAs Array[Double](0.0, 0.0, 0.0)
    leafInQuestion.distribution.toArray should contain theSameElementsInOrderAs Array[Double](0.0, 0.0, 0.0)

    Tree.loadHistory(tree, "ab")
    leafInQuestion = root
    leafInQuestion.totalCounts          should be (2)
    leafInQuestion.frequency.toArray    should contain theSameElementsInOrderAs Array[Double](0.0, 2.0, 0.0)
    leafInQuestion.distribution.toArray should contain theSameElementsInOrderAs Array[Double](0.0, 1.0, 0.0)

    leafInQuestion = rootB
    leafInQuestion.totalCounts          should be (1)
    leafInQuestion.frequency.toArray    should contain theSameElementsInOrderAs Array[Double](1.0, 0.0, 0.0)
    leafInQuestion.distribution.toArray should contain theSameElementsInOrderAs Array[Double](1.0, 0.0, 0.0)

    val rootBA = leafInQuestion.children.head
    leafInQuestion = rootBA
    leafInQuestion.totalCounts          should be (0)
    leafInQuestion.frequency.toArray    should contain theSameElementsInOrderAs Array[Double](0.0, 0.0, 0.0)
    leafInQuestion.distribution.toArray should contain theSameElementsInOrderAs Array[Double](0.0, 0.0, 0.0)

    Tree.loadHistory(tree, "a")
    leafInQuestion = root
    leafInQuestion.totalCounts          should be (3)
    leafInQuestion.frequency.toArray    should contain theSameElementsInOrderAs Array[Double](1.0, 2.0, 0.0)
    leafInQuestion.distribution.toArray should contain theSameElementsInOrderAs Array[Double](1/3, 2/3, 0.0)

    val rootA = leafInQuestion.children.last
    leafInQuestion = rootA
    leafInQuestion.totalCounts          should be (0)
    leafInQuestion.frequency.toArray    should contain theSameElementsInOrderAs Array[Double](0, 0, 0)
    leafInQuestion.distribution.toArray should contain theSameElementsInOrderAs Array[Double](0, 0, 0)
  }

  behavior of "loadData"

  it should "load all complete moving windows for 'abc' (lMax==3): abc, ab, bc, a, b, c" in {
    tree = Tree.loadData(abc.toArray, 3)

    var children:ListBuffer[Leaf] = tree.root.children
    assertChildrenByExactBatch(children, testMap(abc)(0))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, testMap(abc)(1))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, testMap(abc)(2))

    children = children.flatMap(_.children)
    children should have size 0
  }

  it should "load all combinations of our alphabet when givin 'abcabc' (lMax==3): abc, bca, cab, ab, bc, ca, a, b, c" in {
    tree = Tree.loadData(abcabc.toArray, 3)

    var children:ListBuffer[Leaf] = tree.root.children
    assertChildrenByExactBatch(children, testMap(abcabc)(0))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, testMap(abcabc)(1))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, testMap(abcabc)(2))

    children = children.flatMap(_.children)
    children should have size 0
  }


  it should """give root-b three children, given 'abcbabcbb' (@lMax==3):
      | abc, bcb, cba, bab, cbb, ab, bc, cb, ba, bb, a, b, c """.stripMargin in {
    tree = Tree.loadData(abcbabcbb.toArray, 3)

    var children:ListBuffer[Leaf] = tree.root.children
    assertChildrenByExactBatch(children, testMap(abcbabcbb)(0))

    assertChildrenByExactBatch(children.filter(_.observation == 'b').head.children, Seq("ab", "cb", "bb"))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, testMap(abcbabcbb)(1))

    assertChildrenByExactBatch(children.filter(_.observation == 'b').flatMap(_.children), Seq("abc", "cba", "cbb"))

    children = children.flatMap(_.children)
    assertChildrenByExactBatch(children, testMap(abcbabcbb)(2))

    children = children.flatMap(_.children)
    children should have size 0
  }

  it should "not add newlines" in {
    tree = Tree.loadData((abcabc+"\r\n").toArray, 3)
    assertChildrenByExactBatch(tree.root.children, testMap(abcabc)(0))
  }

  def assertChildrenByExactBatch(children:ListBuffer[Leaf], expected:Seq[String]) = {
    children should have size expected.length
    children.map(_.observed)    should contain theSameElementsAs expected
    children.map(_.observation) should contain theSameElementsAs expected.map(_.head)
  }

  behavior of "navigateHistory"

  it should "be able to return the correct leaf" in {
    tree = Tree.loadData(abc.toArray, 3)
    val maybeABC = tree.navigateHistory("abc".toList)
    maybeABC should not be empty
    assertLeafProperties(maybeABC.get, "abc")

    val notPresentA = tree.navigateHistory("abca".toList)
    notPresentA shouldBe empty

    tree = Tree.loadData(abcbabcbb.toArray, 3)

    val maybe_BB = tree.navigateHistory("bb".toList)
    maybe_BB should not be empty
    assertLeafProperties(maybe_BB.get, "bb")

    val maybeCBB = tree.navigateHistory("cbb".toList)
    maybeCBB should not be empty
    assertLeafProperties(maybeCBB.get, "cbb")

    maybe_BB.get.children.map(_.observed) should contain (maybeCBB.get.observed)

    tree = Tree.loadData("aabbabcab".toArray, 3)

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

  behavior of "getDepth"

  it should "be able to return the correct leaf" in {
    tree = Tree.loadData(abcabc.toArray, 3)

    tree.getDepth(0).map(_.observed) should contain only ""

    tree.getDepth(1).map(_.observed) should contain theSameElementsAs testMap(abcabc)(0)

    tree.getDepth(2).map(_.observed) should contain theSameElementsAs testMap(abcabc)(1)

    tree.getDepth(3).map(_.observed) should contain theSameElementsAs testMap(abcabc)(2)

    tree.getDepth(4) should have size 0
  }
}
