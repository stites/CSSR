package com.typeclassified.cssr.cli

import java.io.File

object Cli {
  val cssr     = "cssr"
  val version  = "v0.1.0"
  val lMax     = "lMax"
  val sig      = "sig"
  val alphabet = "alphabet"
  val data     = "data"

  val file = "[file]"
  val value = "[value]"

  val parser = new scopt.OptionParser[Config](Cli.cssr) {
    head(Cli.cssr, Cli.version)

    opt[File](Cli.alphabet.head, Cli.alphabet)
      .required()
      .valueName(file)
      .action { (x, c) => c.copy(alphabetFile = x) }
      .text(s"Required. The ${Cli.alphabet} file for the given data")

    opt[File](Cli.data.head, Cli.data)
      .required()
      .valueName(file)
      .action { (x, c) => c.copy(dataFile = x) }
      .text(s"Required. The ${Cli.data} file of observed sequence")

    opt[Int](Cli.lMax.head, Cli.lMax)
      .valueName(value)
      .action { case (x, c) => c.copy(lMax = x) }
      .validate { x => if (x > 0) success else failure(s"Value <${Cli.lMax}> must be > 0") }
      .text(s"${Cli.lMax} is the maximum size of a history. Defaults to 5")

    opt[Double](Cli.sig.head, Cli.sig)
      .valueName(value)
      .action { (x,c) => c.copy(sig = x) }
      .validate { x => if (x > 0 && x < 1) success else failure(s"Value <${Cli.sig}> must be > 0 && < 1") }
      .text(s"${Cli.sig} is the significance level used for hypothesis testing. Defaults to 0.7")
  }
}
