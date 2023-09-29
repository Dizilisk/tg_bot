ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.9"

lazy val root = (project in file("."))
  .settings(
    name := "tg bot"
  )

val http4sVersion = "0.23.23"

libraryDependencies += "org.augustjune" %% "canoe" % "0.6.0"

libraryDependencies += "org.tpolecat" %% "doobie-core" % "1.0.0-RC1"

libraryDependencies += "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC1"

libraryDependencies += "org.typelevel" %% "log4cats-slf4j"   % "2.5.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.5" % Runtime

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"

libraryDependencies += "org.flywaydb" % "flyway-core" % "9.10.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % Test

libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test

libraryDependencies += "org.scalamock" %% "scalamock" % "5.2.0" % Test

libraryDependencies += "org.postgresql" % "postgresql" % "42.6.0"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-ember-client" % http4sVersion,
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "org.http4s" %% "http4s-dsl"          % http4sVersion,
)

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-circe" % http4sVersion,
  // Optional for auto-derivation of JSON codecs
  "io.circe" %% "circe-generic" % "0.14.5",
  // Optional for string interpolation to JSON model
  "io.circe" %% "circe-literal" % "0.14.5"
)




enablePlugins(DockerPlugin)

//Packing jar
assembly / mainClass := Some("Main")
assembly / assemblyJarName := "baka_bot.jar"

docker / dockerfile := {
  new Dockerfile {
    val artifact: File     = assembly.value
    val artifactTargetPath = s"/app/${artifact.name}"
    from("openjdk:8-jre")
    add(artifact, artifactTargetPath)
    entryPoint("java", "-jar", artifactTargetPath)
  }
}

docker / imageNames := Seq(
  ImageName(
    namespace = Some("Dizi"),
    repository = "local-proxy"
  )
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs@_*) =>
    (xs map {_.toLowerCase}) match {
      case "services" :: xs =>
        MergeStrategy.filterDistinctLines
      case _ => MergeStrategy.discard
    }
  case _                        => MergeStrategy.first
}