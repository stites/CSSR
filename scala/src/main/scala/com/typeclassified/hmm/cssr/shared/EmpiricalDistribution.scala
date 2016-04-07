package com.typeclassified.hmm.cssr.shared

import breeze.linalg.DenseVector
import com.typeclassified.hmm.cssr.trees.ParseLeaf

trait EmpiricalDistribution extends Probablistic {
  var histories: List[ParseLeaf] = List()

  def addHistory(h: ParseLeaf): Unit = addHistories(List(h))

  def addHistories(hs: List[ParseLeaf]): Unit = {
    histories = histories ++ hs
    recalculateHists(histories, DenseVector.zeros[Double](size))
  }

  def rmHistory(x: ParseLeaf): Unit = {
    histories = histories.filter(y => y.observed != x.observed)
    recalculateHists(histories, DenseVector.zeros[Double](size))
  }
}
