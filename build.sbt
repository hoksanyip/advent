name         := "advent"
scalaVersion := "3.1.0"

libraryDependencies ++= Seq(
  "org.typelevel"         %% "cats-core"     % "2.6.1",
  "org.typelevel"         %% "cats-effect"   % "3.2.9",
  "co.fs2"                %% "fs2-core"      % "3.2.2",
  "co.fs2"                %% "fs2-io"        % "3.2.2",
  "com.typesafe"          % "config"         % "1.4.1",
  "io.circe"              %% "circe-core"    % "0.14.1",
  "io.circe"              %% "circe-generic" % "0.14.1",
  "io.circe"              %% "circe-parser"  % "0.14.1",
)
