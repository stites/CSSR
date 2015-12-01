package com.typeclassified.cssr.cli

import java.io.File

case class Config (alphabetFile: File = new File("."),
                   dataFile: File = new File("."),
                   lMax: Int = 5,
                   sig: Double = 0.2
                  ) {
}
