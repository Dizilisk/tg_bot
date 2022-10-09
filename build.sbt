ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.9"

lazy val root = (project in file("."))
  .settings(
    name := "tg bot"
  )

libraryDependencies += "org.augustjune" %% "canoe" % "0.6.0"

libraryDependencies += "org.tpolecat" %% "doobie-core" % "1.0.0-RC1"

libraryDependencies += "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC1"