name                  := "advent"
Global / scalaVersion := "3.1.0"

lazy val dependencies = Seq(
  "org.typelevel" %% "cats-core"   % "2.6.1",
  "org.typelevel" %% "cats-effect" % "3.2.9",
  "co.fs2"        %% "fs2-core"    % "3.2.2",
  "co.fs2"        %% "fs2-io"      % "3.2.2",
  "org.scalanlp"  %% "breeze"      % "2.0" exclude ("org.typelevel", "spire_2.13")
)

lazy val jsonDeps = Seq(
  "com.typesafe"                   % "config"        % "1.4.1",
  "io.circe"                      %% "circe-core"    % "0.14.1",
  "io.circe"                      %% "circe-generic" % "0.14.1",
  "io.circe"                      %% "circe-parser"  % "0.14.1",
  "com.softwaremill.sttp.client3" %% "core"          % "3.3.18"
)

lazy val y2020 = (project in file("2020/scala"))
  .settings(libraryDependencies ++= dependencies)

lazy val y2021 = (project in file("2021/scala"))
  .settings(libraryDependencies ++= dependencies)
