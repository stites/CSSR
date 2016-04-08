package com.typeclassified.hmm.cssr.shared

import breeze.linalg.DenseVector
import com.typeclassified.hmm.cssr.trees.ParseLeaf

trait EmpiricalDistribution extends Probablistic {
  var histories: Set[ParseLeaf] = Set()

  def addHistory(h: ParseLeaf): Unit = addHistories(Set(h))

  def addHistories(hs: Set[ParseLeaf]): Unit = {
    histories = histories ++ hs
    recalculateHists(histories, DenseVector.zeros[Double](size))
  }

  def rmHistory(x: ParseLeaf): Unit = {
    histories = histories.filter(y => y.observed != x.observed)
    recalculateHists(histories, DenseVector.zeros[Double](size))
  }

  def pruneHistories(depth:Int):Unit = {
    histories = histories.filter(y => y.observed.length >= depth)
    recalculateHists(histories, DenseVector.zeros[Double](size))
  }
}
