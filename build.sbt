name := "grappolo"

version := "0.1"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-log4j12" % "1.7.12",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.apache.lucene" % "lucene-spellchecker" % "3.6.2" % "test"
)

crossScalaVersions := Seq("2.10.4", "2.11.6")
