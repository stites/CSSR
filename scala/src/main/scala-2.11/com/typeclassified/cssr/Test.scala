package com.typeclassified.cssr

import com.typeclassified.cssr.parse.ParseNode

import scala.collection.mutable.ListBuffer

object Test {
  def test(S:ListBuffer[CSSRState],
           p:Double,
           aXt:ParseNode,
           s:CSSRState,
           sig:Double
          ) = {

    if (nullHypothesis(s, aXt) > sig) {
      if (!s.histories.contains(aXt)) {
        aXt.changeState(s)
        s.addHistory(aXt)
      }
    } else {
      val sStar:Option[CSSRState] = restrictedHypothesesTesting(S.toList, s, aXt, sig)
      if (sStar.nonEmpty) {
        move(aXt, s, sStar.get)
      } else {
        var sNew = CSSRState()
        S += sNew
        move(aXt, s, sNew)
      }
    }
  }

  def nullHypothesis(s:CSSRState, aXt:ParseNode): Double = {
    ksTest(s.normalized, s.counts, aXt.normalized, aXt.total_counts)
  }

  def restrictedHypothesesTesting( S:List[CSSRState],
                                   s:CSSRState,
                                   ax:ParseNode,
                                   sig:Double
                                 ): Option[CSSRState] = {
    val SStar = S.filter(_ != s)
    for (sStar <- SStar) {
      if (nullHypothesis(sStar, ax) > sig) {
        return Option.apply(sStar)
      }
    }
    Option.empty
  }

  def move(x:ParseNode, s1:CSSRState, s2:CSSRState): Unit = {
    s1.rmHistory(x)
    s2.addHistory(x)
  }
}
