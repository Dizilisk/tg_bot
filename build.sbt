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

libraryDependencies += "org.flywaydb" % "flyway-core" % "9.14.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % Test

libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test

libraryDependencies += "org.scalamock" %% "scalamock" % "5.2.0" % Test


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
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}