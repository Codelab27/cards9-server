name := """cards9-server"""

version := "0.1"

lazy val cards9 = (project in file("."))
  .enablePlugins(PlayScala)
  .disablePlugins(PlayFilters)
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings
  )

scalaSource in IntegrationTest := baseDirectory.value / "it"

resourceDirectory in IntegrationTest := baseDirectory.value / "it/resources"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused",
  "-Ypartial-unification"
)

// https://github.com/playframework/playframework/issues/7832#issuecomment-336014319
dependencyOverrides ++= Seq(
  "com.typesafe.akka" %% "akka-actor"   % "2.5.6",
  "com.typesafe.akka" %% "akka-stream"  % "2.5.6",
  "com.google.guava"   % "guava"        % "22.0",
  "org.slf4j"          % "slf4j-api"    % "1.7.25"
)

libraryDependencies ++= Seq(
    guice,
    "com.typesafe.play"           %% "play-slick"                   % "3.0.1",
    "com.typesafe.play"           %% "play-slick-evolutions"        % "3.0.1",
    "com.typesafe.play"           %% "play-json-joda"               % "2.6.6",
    "io.kanaka"                   %% "play-monadic-actions"         % "2.1.0",
    "io.kanaka"                   %% "play-monadic-actions-cats"    % "2.1.0",
    "org.postgresql"               % "postgresql"                   % "42.1.4",
    "com.h2database"               % "h2"                           % "1.4.196",
    "org.scalactic"               %% "scalactic"                    % "3.0.4",
    "org.scalatest"               %% "scalatest"                    % "3.0.4"       % "it,test",
    "org.scalatestplus.play"      %% "scalatestplus-play"           % "3.1.2"       % "it,test",
    "org.scalacheck"              %% "scalacheck"                   % "1.13.4"      % "it,test",
    "com.beachape"                %% "enumeratum"                   % "1.5.12",
    "org.typelevel"               %% "cats-core"                    % "1.0.0",
    "com.github.julien-truffaut"  %% "monocle-core"                 % "1.4.0"
)

fork in run := true

fork in test := false

packageName in Docker := packageName.value

version in Docker := version.value

enablePlugins(JavaAppPackaging,DockerPlugin)
