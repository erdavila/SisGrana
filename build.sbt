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
  "com.softwaremill.quicklens" %% "quicklens" % "1.7.3",

  "org.scalatest" %% "scalatest" % "3.2.2" % "test"
)
