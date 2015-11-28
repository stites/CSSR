name := "cssr"

organization := "com.typeclassified"

version := "0.1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.1.3",

  "org.scalanlp" %% "breeze"            % "0.11.2", // numerical processing library
  "org.scalanlp" %% "breeze-natives"    % "0.11.2", // greatly improve performance, but increase jar sizes
  // "org.jliszka"  %% "probability-monad" % "1.0.1",  // create, manipulate and sample probability distributions
  "org.scalaz"   %% "scalaz-core"       % "7.1.5",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
)
