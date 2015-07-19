// Comment to get more information during initialization
// logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
//addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.4")

addSbtPlugin("com.vmunier" % "sbt-play-scalajs" % "0.2.4")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.0")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.5.0")
