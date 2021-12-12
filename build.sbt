name                  := "advent"
Global / scalaVersion := "3.1.0"

lazy val dependencies = Seq(
  "org.typelevel"         %% "cats-core"     % "2.6.1",
  "org.typelevel"         %% "cats-effect"   % "3.2.9",
  "co.fs2"                %% "fs2-core"      % "3.2.2",
  "co.fs2"                %% "fs2-io"        % "3.2.2",
  "com.typesafe"          % "config"         % "1.4.1",
  "io.circe"              %% "circe-core"    % "0.14.1",
  "io.circe"              %% "circe-generic" % "0.14.1",
  "io.circe"              %% "circe-parser"  % "0.14.1",
)

lazy val core = (project in file("."))
  .settings(
    libraryDependencies ++= dependencies
  )

lazy val y2020 = (project in file("2020"))
  .dependsOn(core)
  .aggregate(core)
  .settings(
    libraryDependencies ++= dependencies
  )

lazy val y2021 = (project in file("2021"))
  .dependsOn(core)
  .aggregate(core)
  .settings(
    libraryDependencies ++= dependencies
  )
