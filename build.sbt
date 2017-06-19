name := "grappolo"

version := "0.1"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.jfree" % "jfreechart" % "1.0.19" % "test",
  "org.apache.lucene" % "lucene-spellchecker" % "3.6.2" % "test",
  "com.lihaoyi" % "ammonite-repl" % "1.0.0-RC8" % "test" cross CrossVersion.full
)

crossScalaVersions := Seq("2.10.4", "2.11.7", "2.12.2")

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

//(fullClasspath in Test) ++= {
//  (updateClassifiers in Test).value
//    .configurations
//    .find(_.configuration == Test.name)
//    .get
//    .modules
//    .flatMap(_.artifacts)
//    .collect{case (a, f) if a.classifier == Some("sources") => f}
//}
