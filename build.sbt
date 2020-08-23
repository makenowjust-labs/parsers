Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / githubOwner := "MakeNowJust-Labo"
ThisBuild / githubRepository := "parsers"

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation",
  "-Wunused"
)

// Scalafix config:
ThisBuild / scalafixScalaBinaryVersion := "2.13"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.4.0"
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.12"

lazy val root = project
  .in(file("."))
  .aggregate(common, stackparse)

def moduleSettings(moduleName: String) = Seq(
    organization := "codes.quine.labo",
    name := s"parsers-$moduleName",
    version := "0.2,0-SNAPSHOT",
    console / initialCommands := s"""
    |import codes.quine.labo.parsers.$moduleName._
    """.stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    // Settings for test:
    libraryDependencies += "io.monix" %% "minitest" % "2.8.2" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )

lazy val common = project
  .in(file("modules/parsers-common"))
  .settings(
    moduleSettings("common"),
  )

lazy val stackparse = project
  .in(file("modules/parsers-stackparse"))
  .settings(
    moduleSettings("stackparse"),
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.2.1",
  )
  .dependsOn(common)
