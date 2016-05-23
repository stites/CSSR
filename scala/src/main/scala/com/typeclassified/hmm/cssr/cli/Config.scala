package com.typeclassified.hmm.cssr.cli

import java.io.File

case class Config (alphabetFile: File = new File("."),
                   dataFile: File = new File("."),
                   lMax: Int = Parser.lMaxDefault,
                   sig: Double = Parser.sigDefault,
                   stateLabels: Boolean = Parser.stateLabelsDefault,
                   out: Boolean = Parser.outDefault,
                   debug: Boolean = false
                  ) {
  override def toString: String = {
    s"""Current Working Directory: ${System.getProperty("user.dir")}
        |Alphabet file: ${alphabetFile.getPath}
        |Data file: ${dataFile.getPath}
        |History Length: $lMax
        |Multi-line mode: ${false}
        |Significance level: $sig
        |Chi-squared test used: ${false}
        |""".stripMargin
  }
}
