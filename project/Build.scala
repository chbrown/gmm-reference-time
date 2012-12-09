import com.github.retronym.SbtOneJar
import sbt._
import Keys._

object TaccHadoopBuild extends Build {
  def standardSettings = Seq(
    exportJars := true
  ) ++ Defaults.defaultSettings

  lazy val main = Project("tacc-hadoop", file(".")) dependsOn(scoobi)
  // lazy val main = Project("tacc-hadoop", file(".")) dependsOn(scoobi, scalabha)

  lazy val scoobi = Project("scoobi", file("scoobi"))

  // lazy val scalabha = RootProject( file("/Users/chbrown/src/Scalabha") )

}
