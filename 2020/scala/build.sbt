name         := "advent.2020"
scalaVersion := "3.1.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"   % "2.6.1",
  "org.typelevel" %% "cats-effect" % "3.2.9",
  "co.fs2"        %% "fs2-core"    % "3.2.2",
  "co.fs2"        %% "fs2-io"      % "3.2.2",
  "org.scalanlp"  %% "breeze"      % "2.0" exclude ("org.typelevel", "spire_2.13")
)
