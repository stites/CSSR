package com.typeclassified.hmm.utils

import java.nio.file.{Path, Files, Paths}

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

  def runPerlScript(scriptName: String, params: String*):Path = {
    val scriptPath:String = getAbsPathFromScriptName(scriptName)
    val resultsPath:Path = Files.createTempDirectory(scriptName)
    val cmd:String = ((scriptPath +: params) ++ Seq(resultsPath.toAbsolutePath.toString + "/")).mkString(" ")

    logger.debug(s"Running $scriptName with command:\n${cmd}")

    (cmd #> System.out).!

    return resultsPath
  }

  def main(): Unit ={
    val resultDir: Path = runPerlScript("even-process", "100")
  }
}

class TestMachines {
}
