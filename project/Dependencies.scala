import sbt._

object Dependencies {

  lazy val scalatest = Seq(
    "org.scalatest" %% "scalatest" % "3.1.0"
  )

  lazy val srcDependencies = scalatest

}
