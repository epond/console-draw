import AssemblyKeys._

name := "Console Draw"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.3.8" % "test"
)

assemblySettings

jarName in assembly := "console-draw.jar"