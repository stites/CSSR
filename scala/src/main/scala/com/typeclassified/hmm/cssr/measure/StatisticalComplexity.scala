package com.typeclassified.hmm.cssr.measure

import breeze.linalg.DenseVector


/**
  * I think this is the Grassberger-Crutchfield-Young "statistical complexity."
  * I'm taking a shot in the dark at what the name might be.
  */
object StatisticalComplexity extends MathUtils {

  def cMu(dist:DenseVector[Double]):Double = (-1) * dist.foldLeft(0d) { (cMu, p) => cMu + (p * log2(p)) }

}
