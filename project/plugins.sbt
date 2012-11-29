resolvers += Resolver.url(
  //"sbt-plugin-releases",
  "artifactory",
  url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.4")

addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.8")


resolvers += Classpaths.typesafeResolver

addSbtPlugin("com.typesafe.startscript" % "xsbt-start-script-plugin" % "0.5.3")
