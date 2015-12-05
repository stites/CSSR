package com.typeclassified.hmm.cssr.test

import com.typeclassified.hmm.cssr.{Leaf, Leaf$, EquivalenceClass}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object Test {
  val logger = Logger(LoggerFactory.getLogger(Test.getClass))

  def test(S: ListBuffer[EquivalenceClass],
           aXt: Leaf,
           s: EquivalenceClass,
           sig: Double
          ): Unit= {
    if (nullHypothesis(s, aXt) > sig) {
      if (!s.histories.contains(aXt)) {
        aXt.changeEquivalenceClass(s)
        s.addHistory(aXt)
      }
    } else {
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

  def nullHypothesis(s: EquivalenceClass, aXt: Leaf): Double = {
    KolmogorovSmirnov
      .test(
        s.distribution,
        s.totalCounts,
        aXt.distribution,
        aXt.totalCounts)
  }

  def restrictedHypothesesTesting(S: List[EquivalenceClass],
                                  s: EquivalenceClass,
                                  ax: Leaf,
                                  sig: Double
                                 ): Option[EquivalenceClass] = {
    val SStar = S.filter(_ != s)
    for (sStar <- SStar) {
      if (nullHypothesis(sStar, ax) > sig) {
        return Option.apply(sStar)
      }
    }
    Option.empty
  }

  def move(x: Leaf, from: EquivalenceClass, to: EquivalenceClass): Unit = {
    // ABC -> A
    // (A->) C->B->A
    //

    x.changeEquivalenceClass(to)
    from.rmHistory(x)
    to.addHistory(x)

  }
}
