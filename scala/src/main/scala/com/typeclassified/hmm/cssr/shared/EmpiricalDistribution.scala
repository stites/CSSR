package com.typeclassified.hmm.cssr.shared

trait EmpiricalDistribution[P <: Probablistic] extends Probablistic {
  var histories: List[P] = List()
}
