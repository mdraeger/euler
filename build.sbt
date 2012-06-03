name := "Euler"

version := "1.0"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

libraryDependencies ++= Seq(
   "org.scalatest" % "scalatest_2.9.1" % "1.8" % "test",
   "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test")
