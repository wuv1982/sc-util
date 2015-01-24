name := """sc-util"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file("."))
.enablePlugins(PlayScala)
.dependsOn(pre)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  cache,
  ws,
  "org.reactivemongo" %% "reactivemongo" % "0.10.5.0.akka23",
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23",
  "org.apache.commons" % "commons-email" % "1.3.1"
)


lazy val pre = ProjectRef(file("../sc-pre"), "root")