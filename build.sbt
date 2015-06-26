name := "grappolo"

version := "0.1"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  //"com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.apache.lucene" % "lucene-spellchecker" % "3.6.2" % "test"
  //"com.lihaoyi" % "ammonite-repl" % "0.3.2" % "test" cross CrossVersion.full
)

crossScalaVersions := Seq("2.10.4", "2.11.6")

//initialCommands in (Test, console) := """ammonite.repl.Repl.run("")"""
