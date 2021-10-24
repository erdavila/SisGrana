name := "SisGrana"

version := "0.1"

scalaVersion := "2.13.6"
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
  "org.xerial" % "sqlite-jdbc" % "3.36.0",
  "io.getquill" %% "quill-jdbc" % "3.8.0",
  "org.typelevel" %% "cats-core" % "2.3.0",
  "com.softwaremill.quicklens" %% "quicklens" % "1.7.4",

  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)
