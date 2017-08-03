// Comment to get more information during initialization
// logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.url(
  "heroku-sbt-plugin-releases",
  url("https://dl.bintray.com/heroku/sbt-plugins/"))(Resolver.ivyStylePatterns)

// Use the Play sbt plugin for Play projects
//addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.0")

resolvers += Classpaths.sbtPluginReleases
// logLevel := Level.Warn

// addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.7.1")


addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.0.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-twirl" % "1.3.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.18")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.1.0")

addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "1.7")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.1.1")

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.5.2")

addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.13")

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC6")

// addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.3.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")

addSbtPlugin("org.duhemm" % "sbt-errors-summary" % "0.3.0")

addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.0.0")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.9.0")
