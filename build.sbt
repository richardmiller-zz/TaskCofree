name := "CoFree"

version := "1.0"

scalaVersion := "2.11.7"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")

libraryDependencies += "org.typelevel" %% "cats" % "0.4.0"

scalacOptions += "-Xlog-implicits"