name := "Puzzle"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.0-SNAP9" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.5" % Test,
  "junit" % "junit" % "4.12" % Test
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_"
)

        