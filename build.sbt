name := "chip-lang"

version := "0.1.0"

scalaVersion := "2.11.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

// Allows SPI to find sound components.
// http://stackoverflow.com/questions/18676712/java-sound-devices-found-when-run-in-intellij-but-not-in-sbt
fork in run := true
