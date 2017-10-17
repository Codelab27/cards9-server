name := """cards9-server"""

version := "0.1"

lazy val cards9 = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.11"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused"
)

routesGenerator := InjectedRoutesGenerator

libraryDependencies ++= Seq(
    "com.typesafe.play" %% "play-slick" % "2.1.0",
    "com.typesafe.play" %% "play-slick-evolutions" % "2.1.0",
    "org.postgresql" % "postgresql" % "42.1.1",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    "com.beachape" %% "enumeratum" % "1.5.12"
)

fork in run := true

packageName in Docker := packageName.value

version in Docker := version.value

enablePlugins(JavaAppPackaging,DockerPlugin)
