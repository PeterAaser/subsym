name := "subsym2"

version := "0.0.1"

scalaVersion := "2.11.7"
 
val scalazVersion = "7.1.0"

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-effect" % scalazVersion,
    "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
    "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
    "org.scalafx" %% "scalafx" % "8.0.60-R9",
    "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4",
    "io.spray" %% "spray-json" % "1.3.2",
    "org.scalafx" %% "scalafx" % "8.0.60-R9"
)

scalacOptions := Seq("-feature", "-deprecation")

// fork in run := true
// cancelable in Global := true

import com.github.retronym.SbtOneJar._

oneJarSettings
