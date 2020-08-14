Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / githubOwner := "MakeNowJust-Labo"
ThisBuild / githubRepository := "scala-labo-miniparse"

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
    name := "miniparse",
    version := "0.1.0",
    console / initialCommands := """
      |import codes.quine.labo.miniparse._
      """.stripMargin,
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Dependencies:
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    // Settings for test:
    libraryDependencies += "io.monix" %% "minitest" % "2.8.2" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )

lazy val bench = project
  .in(file("bench"))
  .settings(
    name := "miniparse-bench",
    console / initialCommands := """
      |import codes.quine.labo.miniparse._
      |import codes.quine.labo.miniparse.bench._
      """.stripMargin,
    scalacOptions ++= Seq(
      "-opt:l:inline",
      "-opt-inline-from:codes.quine.labo.miniparse.*",
      "-opt-warnings"
    ),
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    libraryDependencies += "org.tpolecat" %% "atto-core" % "0.7.0"
  )
  .dependsOn(root)
  .enablePlugins(JmhPlugin)
