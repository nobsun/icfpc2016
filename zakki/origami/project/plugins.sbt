// resolvers ++= Seq(
//   Classpaths.sbtPluginReleases,
//   Resolver.url("sbt-scoverage repo", url("https://dl.bintray.com/sksamuel/sbt-plugins"))(Resolver.ivyStylePatterns)
// )

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "4.0.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
