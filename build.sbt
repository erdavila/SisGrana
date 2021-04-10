name := "SisGrana"

version := "0.1"

scalaVersion := "2.13.4"
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Ymacro-annotations",
)

idePackagePrefix := Some("sisgrana")

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %% "monocle-core"  % "3.0.0-M4",
  "com.github.julien-truffaut" %% "monocle-macro" % "3.0.0-M4",

  "org.scalatest" %% "scalatest" % "3.2.2" % "test"
)
