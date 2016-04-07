package com.typeclassified.hmm.cssr.shared

import breeze.linalg.{sum, DenseVector}
import breeze.numerics.{abs, rint}
import com.typeclassified.hmm.cssr.parse.AlphabetHolder
import com.typeclassified.hmm.cssr.trees.Tree

trait Probablistic {
  implicit val ep:Epsilon = new Epsilon(0.01)

  protected val size: Int = AlphabetHolder.alphabet.map.size // this is the thorn in my side

  protected val zeros: DenseVector[Double] = DenseVector.zeros(size)

  var totalCounts: Double = 0
  var frequency: DenseVector[Double] = zeros
  var distribution: DenseVector[Double] = zeros
  var rounded: DenseVector[Double] = zeros

  def recalculate(counts:Array[Double]):Unit = recalculate(List(new DenseVector(counts)))

  def recalculate(newInputs:List[Probablistic]):Unit = recalculate(newInputs.map{ _.frequency })

  def recalculate(newInputs:Iterable[DenseVector[Double]]):Unit = {
    frequency = newInputs.foldLeft(frequency)(_+_)
    totalCounts = sum(frequency)
    distribution = if (totalCounts > 0) frequency  / totalCounts else zeros
    rounded      = if (totalCounts > 0) Tree.round(distribution) else zeros
  }

  def round(u:Probablistic): DenseVector[Double] = round(u.distribution)

  def round(dist:DenseVector[Double]): DenseVector[Double] = {
    val rndPrecision:Double = 1 / ep.precision
    (rint(dist * rndPrecision):DenseVector[Double]) / rndPrecision
  }

  def ~= (p:Probablistic)(implicit ep:Epsilon):Boolean = {
    (abs(distribution :- p.distribution) :< ep.precision).reduceRight(_&&_)
  }
}
