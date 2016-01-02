package com.typeclassified.hmm.cssr.cli

import java.io.File

case class Config (alphabetFile: File = new File("."),
                   dataFile: File = new File("."),
                   lMax: Int = Parser.lMaxDefault,
                   sig: Double = Parser.sigDefault
                  ) {
}
