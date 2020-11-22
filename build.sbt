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
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.4.3"
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.16"

lazy val root = project
  .in(file("."))
  .settings(publish / skip := true)
  .aggregate(bench, common, contparse, funcparse, inlineparse, stackparse)

lazy val bench = project
  .in(file("modules/bench"))
  .settings(
    publish / skip := true,
    console / initialCommands := s"""
    |import codes.quine.labo.parsers.bench._
    """.stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    scalacOptions ++= Seq(
      "-opt:l:inline",
      "-opt-inline-from:codes.quine.labo.parsers.inlineparse.*",
      "-opt-warnings"
    ),
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    libraryDependencies += "org.tpolecat" %% "atto-core" % "0.8.0",
    // Settings for test:
    libraryDependencies += "io.monix" %% "minitest" % "2.9.0" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )
  .dependsOn(contparse, funcparse, inlineparse, stackparse)
  .enablePlugins(JmhPlugin)

def moduleSettings(moduleName: String) =
  Seq(
    organization := "codes.quine.labo",
    name := s"parsers-$moduleName",
    version := "0.2.2-SNAPSHOT",
    console / initialCommands := s"""
    |import codes.quine.labo.parsers.$moduleName._
    """.stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    // Settings for test:
    libraryDependencies += "io.monix" %% "minitest" % "2.9.0" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )

lazy val common = project
  .in(file("modules/parsers-common"))
  .settings(moduleSettings("common"))

lazy val contparse = project
  .in(file("modules/parsers-contparse"))
  .settings(
    moduleSettings("contparse"),
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.2.1"
  )
  .dependsOn(common)

lazy val funcparse = project
  .in(file("modules/parsers-funcparse"))
  .settings(
    moduleSettings("funcparse"),
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.2.1"
  )
  .dependsOn(common)

lazy val inlineparse = project
  .in(file("modules/parsers-inlineparse"))
  .settings(
    moduleSettings("inlineparse"),
    // Dependencies:
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value
  )
  .dependsOn(common)

lazy val stackparse = project
  .in(file("modules/parsers-stackparse"))
  .settings(
    moduleSettings("stackparse"),
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.2.1"
  )
  .dependsOn(common)
