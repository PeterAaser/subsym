name := "subsym2"

version := "0.0.1"

scalaVersion := "2.11.2"
 
val scalazVersion = "7.1.0"

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-effect" % scalazVersion,
    "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
    "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
    "org.scalafx" %% "scalafx" % "8.0.60-R9"
)

fork in run := true

import com.github.retronym.SbtOneJar._

oneJarSettings
