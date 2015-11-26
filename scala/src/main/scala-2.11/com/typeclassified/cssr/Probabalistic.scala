package com.typeclassified.cssr

import breeze.linalg.DenseVector
import com.typeclassified.cssr.parse.AlphabetHolder

trait Probablistic {
  var frequency:DenseVector[Double] = DenseVector.zeros[Double](AlphabetHolder.alphabet.size)
  var normalDistribution:DenseVector[Double] = DenseVector.zeros[Double](AlphabetHolder.alphabet.size)
  var totalCounts:Int = 0
}
