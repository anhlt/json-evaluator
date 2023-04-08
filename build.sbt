import Dependencies._

ThisBuild / scalaVersion := "3.2.2"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

val commonSettings = Seq(
  scalacOptions -= "-Xfatal-warnings",
  // scalacOptions += "-source:future",
  libraryDependencies ++= Dependencies.all
)

lazy val root = (project in file("."))
  .settings(
    name := "Json Evaluator"
  )
  .settings(
    commonSettings
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
