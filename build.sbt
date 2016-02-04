name := "CoFree"

version := "1.0"

scalaVersion := "2.11.7"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")

lazy val monocleVersion = "1.1.1"

libraryDependencies ++= {
  Seq(
    "org.typelevel" %% "cats" % "0.4.0",
    "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
    "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
    "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
  )
}


scalacOptions += "-Xlog-implicits"