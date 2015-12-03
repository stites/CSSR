name := "cssr"

organization := "com.typeclassified"

version := "0.1.0"

runMain := Some("com.typeclassified.hmm.cssr.CSSR")

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "ch.qos.logback"             % "logback-classic" % "1.1.3", // add logging
  "com.typesafe.scala-logging" %% "scala-logging"  % "3.1.0", // and logging in scala
  "com.github.scopt"           %% "scopt"          % "3.3.0", // commandline parsing

  "org.scalaz"    %% "scalaz-core"       % "7.1.5",  // primarily for managing state - not yet implimented
  "org.scalanlp"  %% "breeze"            % "0.11.2", // numerical processing library
  "org.scalanlp"  %% "breeze-natives"    % "0.11.2", // greatly improve performance, but increase jar sizes
  // "org.jliszka"  %% "probability-monad" % "1.0.1",  // create, manipulate and sample probability distributions
  "org.scalatest" %% "scalatest"         % "2.2.4" % "test" // for testing

)

