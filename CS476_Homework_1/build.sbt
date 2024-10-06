ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.1"

lazy val root = (project in file("."))
  .settings(
    name := "CS476_Homework_1",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "latest.release" % Test,
      "org.scalatest" %% "scalatest-featurespec" % "latest.release" % Test
    )
  )
