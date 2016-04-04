package com.typeclassified.hmm.cssr.test

import com.typeclassified.hmm.cssr.shared.{Level, Logging}
import com.typeclassified.hmm.cssr.state.EquivalenceClass
import com.typeclassified.hmm.cssr.test.hypothesis.{KolmogorovSmirnov=>KS}
import com.typeclassified.hmm.cssr.trees.ParseLeaf

import scala.collection.mutable.ListBuffer

object Test extends Logging {
  override def loglevel() = Level.OFF

  def test(S: ListBuffer[EquivalenceClass], aXt: ParseLeaf, parent:ParseLeaf, s: EquivalenceClass, sig: Double): Unit = {
    debug(s"Total number of states: ${S.length}")
    if (nullHypothesis(s, aXt, sig) >= sig) {
      if (!s.histories.contains(aXt)) {
        aXt.changeEquivalenceClass(s)
        s.addHistory(aXt)
      }
    } else {
      debug("Rejecting null hypothesis")
      val sStar: Option[EquivalenceClass] = restrictedHypothesesTesting(S.toList, s, aXt, sig)
      if (sStar.nonEmpty) {
        move(aXt, s, parent, sStar.get, false)
      } else {
        info(s"Generating a new equivalence class with: ${aXt.observation}")
        var sNew = EquivalenceClass()
        S += sNew
        move(aXt, s, parent, sNew, true)
      }
    }
    S --= S.filter(_.histories.isEmpty)
  }

  def nullHypothesis(s: EquivalenceClass, aXt: ParseLeaf, sig:Double): Double = {
    debug("Testing: " + aXt.toString)
    debug(s"Have state information:")
    debug("======================================")
    s.histories.foreach{ h => debug(h.toString) }
    debug("======================================")
    debug(s"Running test -- Count1: ${s.totalCounts} Count2: ${aXt.totalCounts}")
    debug(s"state: ${s.distribution.toString}\t\tfreq1: ${s.frequency.toString()}")
    debug(s" leaf: ${aXt.distribution.toString}\t\tfreq2: ${aXt.frequency.toString()}")
    KS.kstwo(s.distribution, s.totalCounts, aXt.distribution, aXt.totalCounts)
  }

  def restrictedHypothesesTesting(S: List[EquivalenceClass], s: EquivalenceClass, ax: ParseLeaf, sig: Double )
  :Option[EquivalenceClass] = {
    val SStar = S.filter(_ ne s)
    for (sStar <- SStar) {
      if (nullHypothesis(sStar, ax, sig) >= sig) {
        return Option(sStar)
      }
    }
    None
  }

  def move(x: ParseLeaf, from: EquivalenceClass, parent:ParseLeaf, to: EquivalenceClass, rmParent:Boolean=true, paint:Boolean = true): Unit = {
    x.changeEquivalenceClass(to, paint)
    to.addHistory(x)
    from.rmHistory(x) // remove history as we have moved to "painting" the parse tree
    if (parent != null && rmParent) from.rmHistory(parent)// remove ancestors as we need to disambiguate if progeny
  }
}
