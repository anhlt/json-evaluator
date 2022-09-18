import sbt._

object Dependencies {

  object Cats {

    val declineVersion = "2.2.0"
    val jacksonVersion = "2.13.3"
    val scribeVersion = "3.10.3"

    val catsEffect = "org.typelevel" %% "cats-effect" % "3.3.11"
    val catsMtl = "org.typelevel" %% "cats-mtl" % "1.2.1"
    val catsParse = "org.typelevel" %% "cats-parse" % "0.3.7"

    val decline = "com.monovore" %% "decline" % declineVersion
    val declineEffect = "com.monovore" %% "decline-effect" % declineVersion
    val jackson =
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % jacksonVersion
    val scalaParser =
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

    val log4cats =
      "org.typelevel" %% "log4cats-core" % "2.5.0" // Only if you want to Support Any Backend
    val log4catsJ =
      "org.typelevel" %% "log4cats-slf4j" % "2.5.0" // Direct Slf4j Support - Recommended

    val scribe = "com.outr" %% "scribe" % scribeVersion
    val scribeCats = "com.outr" %% "scribe-cats" % scribeVersion

    val all = Seq(
      catsEffect,
      catsMtl,
      catsParse,
      declineEffect,
      jackson,
      scalaParser,
      log4cats,
      log4catsJ,
      scribe,
      scribeCats
    )
  }

  object Tests {
    val munit = "org.scalameta" %% "munit" % "0.7.29" % Test
    val munitCatsEffect =
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
    val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.16.0" % Test
    val munitScalaCheck =
      "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
    val scalacheck =
      "org.scalatestplus" %% "scalacheck-1-16" % "3.2.12.0" % "test"

    val all = Seq(
      munit,
      munitScalaCheck,
      munitCatsEffect,
      scalaCheck,
      scalacheck
    )
  }

  val all = Cats.all ++ Tests.all

}
