name := """sc-util"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file("."))
.enablePlugins(PlayScala)
.dependsOn(pre)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  cache,
  ws
)


lazy val pre = ProjectRef(file("../sc-pre"), "root")