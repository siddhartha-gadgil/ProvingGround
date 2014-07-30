import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "ProvingGround"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
//	playAssetsDirectories <+= baseDirectory / "/web",
//	playAssetsDirectories <+= baseDirectory / "/packages",
//	playAssetsDirectories <+= baseDirectory / "/build",
//	libraryDependencies ++= Seq(
//  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.2"
//)
  )

}
