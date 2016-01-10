package com.typeclassified.hmm.cssr.test

import com.typeclassified.hmm.cssr.parse.Leaf
import com.typeclassified.hmm.cssr.state.EquivalenceClass
import com.typeclassified.hmm.cssr.test.hypothesis.{KolmogorovSmirnov=>KS}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object Test {
  val logger = Logger(LoggerFactory.getLogger(Test.getClass))

  def test(S: ListBuffer[EquivalenceClass], aXt: Leaf, s: EquivalenceClass, sig: Double): Unit= {
    logger.debug(s"Total number of states: ${S.length}")
    if (nullHypothesis(s, aXt, sig)) {
      if (!s.histories.contains(aXt)) {
        aXt.changeEquivalenceClass(s)
        s.addHistory(aXt)
      }
    } else {
      logger.debug("Rejecting null hypothesis")
      val sStar: Option[EquivalenceClass] = restrictedHypothesesTesting(S.toList, s, aXt, sig)
      if (sStar.nonEmpty) {
        move(aXt, s, sStar.get)
      } else {
        logger.info(s"Generating a new equivalence class with: ${aXt.observation}")
        var sNew = EquivalenceClass()
        S += sNew
        move(aXt, s, sNew)
      }
    }
  }

  def nullHypothesis(s: EquivalenceClass, aXt: Leaf, sig:Double): Boolean = {
    logger.debug("Testing: " + aXt.toString)
    logger.debug(s"Have state information:")
    logger.debug("======================================")
    s.histories.foreach{ h => logger.debug(h.toString) }
    logger.debug("======================================")
    logger.debug(s"Running test -- Count1: ${s.totalCounts} Count2: ${aXt.totalCounts}")
    logger.debug(s"state: ${s.distribution.toString}\t\tfreq1: ${s.frequency.toString()}")
    logger.debug(s" leaf: ${aXt.distribution.toString}\t\tfreq2: ${aXt.frequency.toString()}")
    KS.kstwo(s.distribution, s.totalCounts, aXt.distribution, aXt.totalCounts) > sig
  }

  def restrictedHypothesesTesting( S: List[EquivalenceClass],
                                   s: EquivalenceClass,
                                   ax: Leaf,
                                   sig: Double )
  :Option[EquivalenceClass] = {
    val SStar = S.filter(_ ne s)
    for (sStar <- SStar) {
      if (nullHypothesis(sStar, ax, sig)) {
        return Option.apply(sStar)
      }
    }
    Option.empty
  }

  def move(x: Leaf, from: EquivalenceClass, to: EquivalenceClass): Unit = {
    x.changeEquivalenceClass(to)
    to.addHistory(x)
    from.rmHistory(x) // remove history as we have moved to "painting" the parse tree
    // TODO: ask kristi or cosma about the following as it may be eliminating too many histories
    if (x.parent.nonEmpty) from.rmHistory(x.parent.get)// remove ancestors as we need to disambiguate if progeny
  }
}
