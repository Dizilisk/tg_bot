ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.9"

lazy val root = (project in file("."))
  .settings(
    name := "tg bot"
  )

libraryDependencies += "org.augustjune" %% "canoe" % "0.6.0"

libraryDependencies += "org.tpolecat" %% "doobie-core" % "1.0.0-RC1"

libraryDependencies += "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC1"

libraryDependencies += "org.typelevel" %% "log4cats-slf4j"   % "2.5.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.5" % Runtime

libraryDependencies += "org.flywaydb" % "flyway-core" % "9.12.0"