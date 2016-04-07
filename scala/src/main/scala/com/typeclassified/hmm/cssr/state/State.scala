package com.typeclassified.hmm.cssr.state

import com.typeclassified.hmm.cssr.shared.Probablistic

trait State[P <: Probablistic] extends Probablistic {
  var histories: List[P] = List()
}
