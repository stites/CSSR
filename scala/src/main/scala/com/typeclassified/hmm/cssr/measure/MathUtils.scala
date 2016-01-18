package com.typeclassified.hmm.cssr.measure

trait MathUtils {
  def log2(x: Double) = math.log(x)/math.log(2)

  def discreteEntropy(a:Double, b:Double):Double = a * log2(a / b)
}
