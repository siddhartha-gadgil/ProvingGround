import sbt._
import Keys._
//import play.Project._

object ApplicationBuild extends Build {

  val appName         = "ProvingGround"
  val appVersion      = "1.0-SNAPSHOT"

/*
  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm
  )
*/

  val appDependencies = Seq("org.reactivemongo" %% "play2-reactivemongo" % "0.10.2")

//  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here
/*
  resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  libraryDependencies ++= Seq(
    "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.akka23-SNAPSHOT"
)*/


  val main = Project(appName, file(".")).enablePlugins(play.PlayScala).settings(
    version := appVersion,
    libraryDependencies ++= appDependencies
  )



}
