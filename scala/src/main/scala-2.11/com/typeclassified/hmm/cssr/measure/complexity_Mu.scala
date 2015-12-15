package com.typeclassified.hmm.cssr.measure

import com.typeclassified.hmm.cssr.state.EquivalenceClass

// taking a shot in the dark at what the name might be implying
// relying on anti-cargo-cult paradigms to indicate something is wrong
object complexity_Mu {
  def calculate_Complexity_mu(allStates:Array[EquivalenceClass]) = {
    /*
    val size:Int = allStates.length
    var cMu :Double = 0
    var prob :Double = 0

    for (state <- allStates) {
      prob = state.frequency
      if (prob) {
        cMu += prob*(log(prob)/log(2));
      }
    }

    for(int i = 0; i < size; i++) {
      state = m_allstates->getState(i);
      prob = state->getFrequency();
      if (prob) {
        cMu += prob*(log(prob)/log(2));
      }
    }
    m_cMu = -cMu;
    */
  }
}
