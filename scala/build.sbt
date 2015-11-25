name := "cssr"

organization := "com.typeclassified"

version := "0.1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze"            % "0.11.2", // numerical processing library
  "org.scalanlp" %% "breeze-natives"    % "0.11.2", // greatly improve performance, but increase jar sizes
  "org.jliszka"  %% "probability-monad" % "1.0.1"   // create, manipulate and sample probability distributions
)
