import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.5",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Swing",

    // libraryDependencies += scalaTest % Test,
    // libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11.0-M7"

    libraryDependencies ++= Seq (
        scalaTest % Test,
        "org.scala-lang.modules" % "scala-swing_2.12" % "2.0.3"
    )
  )
