// Comment to get more information during initialization
// logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.url("heroku-sbt-plugin-releases",
  url("https://dl.bintray.com/heroku/sbt-plugins/"))(Resolver.ivyStylePatterns)

// Use the Play sbt plugin for Play projects
//addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.5")

addSbtPlugin("com.vmunier" % "sbt-play-scalajs" % "0.2.7")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.2")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.5.0")

addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.7")
