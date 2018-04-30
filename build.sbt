val Http4sVersion = "0.18.9"
val Specs2Version = "4.1.0"
val LogbackVersion = "1.2.3"

lazy val root = {
  project
    .in(file("."))
    .settings(
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")
    )
    .settings(
      organization := "com.example",
      name := "spike",
      version := "0.0.1-SNAPSHOT",
      scalaVersion := "2.12.5",
      libraryDependencies ++= Seq(
        "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
        "org.http4s"      %% "http4s-circe"        % Http4sVersion,
        "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
        "org.http4s"      %% "rho-swagger"          % "0.18.0",
        "org.specs2"     %% "specs2-core"          % Specs2Version % "test",
        "ch.qos.logback"  %  "logback-classic"     % LogbackVersion
      )
    )
}

