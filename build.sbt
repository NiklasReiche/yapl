name := "myscript"

version := "0.1"

scalaVersion := "2.13.2"

// testing library
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8"

// run tests sequentially, so you know which one failed.
parallelExecution in Test := false

// more error checking
scalacOptions ++= Seq("-unchecked", "-deprecation")
