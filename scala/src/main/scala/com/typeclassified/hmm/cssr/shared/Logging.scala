package com.typeclassified.hmm.cssr.shared

import com.typeclassified.hmm.cssr.shared.Level.Level
import com.typesafe.scalalogging.{Logger, LazyLogging}

import org.slf4j.{Marker, LoggerFactory}

object Level extends Enumeration {
  type Level = Value
  val OFF, ERROR, WARN, INFO, DEBUG = Value
}

trait Logging extends LazyLogging {
  protected lazy val _log: Logger = Logger(LoggerFactory.getLogger(getClass.getName))

  // Stupid hack to get unstuck on this. I think we can clean this up with implicits later
  def loglevel() = Level.INFO

  def guard(level: Level):Boolean = loglevel() >= level

  def guardDebug() = guard(Level.DEBUG)
  def guardInfo() = guard(Level.INFO)
  def guardWarn() = guard(Level.WARN)
  def guardError() = guard(Level.ERROR)

  def debug(message:String):Unit                                  = if (guardDebug()) _log.debug(message)
  def debug(message:String, args:AnyRef*):Unit                    = if (guardDebug()) _log.debug(message, args)
  def debug(message:String, cause:Throwable):Unit                 = if (guardDebug()) _log.debug(message, cause)
  def debug(marker: Marker, message:String):Unit                  = if (guardDebug()) _log.debug(marker, message)
  def debug(marker: Marker, message:String, args:AnyRef*):Unit    = if (guardDebug()) _log.debug(marker, message, args)
  def debug(marker: Marker, message:String, cause:Throwable):Unit = if (guardDebug()) _log.debug(marker, message, cause)

  def info(message:String):Unit                                   = if (guardInfo())  _log.info(message)
  def info(message:String, args:AnyRef*):Unit                     = if (guardInfo())  _log.info(message, args)
  def info(message:String, cause:Throwable):Unit                  = if (guardInfo())  _log.info(message, cause)
  def info(marker: Marker, message:String):Unit                   = if (guardInfo())  _log.info(marker, message)
  def info(marker: Marker, message:String, args:AnyRef*):Unit     = if (guardInfo())  _log.info(marker, message, args)
  def info(marker: Marker, message:String, cause:Throwable):Unit  = if (guardInfo())  _log.info(marker, message, cause)

  def warn(message:String):Unit                                   = if (guardWarn())  _log.warn(message)
  def warn(message:String, args:AnyRef*):Unit                     = if (guardWarn())  _log.warn(message, args)
  def warn(message:String, cause:Throwable):Unit                  = if (guardWarn())  _log.warn(message, cause)
  def warn(marker: Marker, message:String):Unit                   = if (guardWarn())  _log.warn(marker, message)
  def warn(marker: Marker, message:String, args:AnyRef*):Unit     = if (guardWarn())  _log.warn(marker, message, args)
  def warn(marker: Marker, message:String, cause:Throwable):Unit  = if (guardWarn())  _log.warn(marker, message, cause)

  def error(message:String):Unit                                  = if (guardError()) _log.error(message)
  def error(message:String, args:AnyRef*):Unit                    = if (guardError()) _log.error(message, args)
  def error(message:String, cause:Throwable):Unit                 = if (guardError()) _log.error(message, cause)
  def error(marker: Marker, message:String):Unit                  = if (guardError()) _log.error(marker, message)
  def error(marker: Marker, message:String, args:AnyRef*):Unit    = if (guardError()) _log.error(marker, message, args)
  def error(marker: Marker, message:String, cause:Throwable):Unit = if (guardError()) _log.error(marker, message, cause)
}
