package com.typeclassified.hmm.cssr

import com.typeclassified.hmm.cssr.cli.{Parser, Config}

object CSSRCli {
  def main(args: Array[String]): Unit = {
    Parser.parser.parse(args, Config()) match {
      case Some(config) => CSSR.run(config)
      case None => {}
    }
  }
}
