package com.typeclassified.hmm.utils

import java.nio.file.Paths

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.sys.process._

/**
  * A utility class to initialize and clean up the test-machine perl scripts.
  */
object TestMachines {
  protected val logger = Logger(LoggerFactory.getLogger(TestMachines.getClass))

  protected def resourceLocation(name:String):String = s"/test-machines/${name}/${name}.pl"

  protected def getAbsPathFromScriptName(name:String):String = {
    val path = TestMachines.resourceLocation(name)
    try {
      Paths.get(TestMachines.getClass.getResource(path).toURI).toFile.getAbsolutePath
    } catch {
      case npe: NullPointerException => throw new RuntimeException(s"Script $name does not exist at location: $path", npe)
    }
  }

  def runPerlScript(scriptName: String, params: String*):Int = {
    logger.debug(s"$scriptName called with", params.toString())

    val cmd:String = (getAbsPathFromScriptName(scriptName) +: params).mkString(" ")
    return (cmd #> System.out).!
  }

  def main(): Unit ={
    runPerlScript("even-process", "100", "AB")
  }
}

class TestMachines {
}
