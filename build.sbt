name := """cards9-server"""

version := "0.1"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

routesGenerator := InjectedRoutesGenerator

libraryDependencies ++= Seq(
    "com.typesafe.play" %% "play-slick" % "2.0.0",
    "com.typesafe.play" %% "play-slick-evolutions" % "2.0.0",
    "org.postgresql" % "postgresql" % "9.4.+",
    "org.scalatest" %% "scalatest" % "3.0.+" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
    "com.beachape" %% "enumeratum" % "1.5.+"
)

fork in run := true
