package com.typeclassified.cssr

import breeze.linalg.DenseVector
import com.typeclassified.cssr.parse.AlphabetHolder

trait Probablistic {
  protected val size: Int = AlphabetHolder.alphabet.size
  var frequency: DenseVector[Double] = DenseVector.zeros(size)
  var distribution: DenseVector[Double] = DenseVector.zeros(size)
  var totalCounts: Double = 0
}
