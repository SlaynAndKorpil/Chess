// https://www.scala-sbt.org/release/docs/Multi-Project.html

name := "Chess"

version := "0.3"

ThisBuild / scalaVersion := "2.12.8"

ThisBuild / mainClass := Some("chess/Main")

ThisBuild / exportJars := true

ThisBuild / libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "2.0.3",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.0"
)

lazy val root = (project in file(".")) dependsOn (console, graphics)

lazy val framework = project

lazy val console = project dependsOn framework

lazy val graphics = project dependsOn framework
