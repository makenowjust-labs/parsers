Global / onChangedBuildSource := ReloadOnSourceChanges

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
    name := "stackparse",
    organization := "codes.quine.labo",
    version := "0.1.0-SNAPSHOT",
    console / initialCommands := """
      |import codes.quine.labo.stackparse._
      """.stripMargin,
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // dependencies:
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.9",
    // test dependencies:
    libraryDependencies += "io.monix" %% "minitest" % "2.8.2" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )

lazy val bench = project
  .in(file("bench"))
  .settings(
    name := "stackparse-bench",
    organization := "codes.quine.labo",
    version := "0.1.0-SNAPSHOT",
    console / initialCommands := """
      |import codes.quine.labo.stackparse._
      |import codes.quine.labo.stackparse.bench._
      """.stripMargin,
    // dependencies:
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  )
  .dependsOn(root)
  .enablePlugins(JmhPlugin)
