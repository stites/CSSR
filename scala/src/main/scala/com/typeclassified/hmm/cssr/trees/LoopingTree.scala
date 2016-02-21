package com.typeclassified.hmm.cssr.trees

import com.typeclassified.hmm.cssr.parse.Alphabet
import com.typeclassified.hmm.cssr.shared.Probablistic

class LoopingTree (alphabet: Alphabet) {
}

class LoopingLeaf (observedSequence:String, parent:Option[LoopingLeaf], tree:LoopingTree) extends Probablistic {
}
