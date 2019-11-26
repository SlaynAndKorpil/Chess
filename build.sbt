name := "Chess"

version := "0.1"

scalaVersion := "2.12.8"

mainClass := Some("chess/Main")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "2.0.3",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.0"
)

lazy val framework = project in file("ChessFramework")

lazy val root = (project in file(".")) dependsOn framework
