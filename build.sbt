name := "Generics And HKT"

version := "1.0"

scalaVersion := "2.13.8"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "com.github.sbt" % "junit-interface" % "0.13.2" % Test,
  "org.scalatest" %% "scalatest" % "3.2.12" % Test,
)
