name := "scala-monads"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "io.frees" %% "frees-core" % "0.8.2"

addCompilerPlugin(
  "org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
