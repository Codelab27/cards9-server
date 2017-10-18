name := """cards9-server"""

version := "0.1"

lazy val cards9 = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.3"

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

// https://github.com/playframework/playframework/issues/7832#issuecomment-336014319
dependencyOverrides ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.6",
  "com.typesafe.akka" %% "akka-stream" % "2.5.6",
  "com.google.guava" % "guava" % "22.0",
  "org.slf4j" % "slf4j-api" % "1.7.25"
)

routesGenerator := InjectedRoutesGenerator

libraryDependencies ++= Seq(
    "com.typesafe.play" %% "play-slick" % "3.0.1",
    "com.typesafe.play" %% "play-slick-evolutions" % "3.0.1",
    "org.postgresql" % "postgresql" % "42.1.4",
    "org.scalactic" %% "scalactic" % "3.0.4",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    "com.beachape" %% "enumeratum" % "1.5.12"
)

fork in run := true

packageName in Docker := packageName.value

version in Docker := version.value

enablePlugins(JavaAppPackaging,DockerPlugin)
