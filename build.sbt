Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / githubOwner := "MakeNowJust-Labo"
ThisBuild / githubRepository := "scala-labo-stackparse"
ThisBuild / githubTokenSource := TokenSource.Environment("GITHUB_TOKEN")

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation"
)

lazy val root = project
  .in(file("."))
  .settings(
    organization := "codes.quine.labo",
    name := "stackparse",
    version := "0.1.1-SNAPSHOT",
    console / initialCommands := """
      |import codes.quine.labo.stackparse._
      """.stripMargin,
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.9",
    // Settings for test:
    libraryDependencies += "io.monix" %% "minitest" % "2.8.2" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )

lazy val bench = project
  .in(file("bench"))
  .settings(
    name := "stackparse-bench",
    console / initialCommands := """
      |import codes.quine.labo.stackparse._
      |import codes.quine.labo.stackparse.bench._
      """.stripMargin,
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  )
  .dependsOn(root)
  .enablePlugins(JmhPlugin)
