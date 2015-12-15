package com.typeclassified.hmm.cssr.state

import scala.collection.mutable.ListBuffer

class Machine (equivalenceClasses: ListBuffer[EquivalenceClass]) {
  val states = equivalenceClasses.toArray
}
