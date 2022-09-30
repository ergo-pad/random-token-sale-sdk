inThisBuild(List(
  
  organization := "io.ergopad",  // groupId
  name := "random-token-sale-sdk", // artifactId
  homepage := Some(url("https://ergopad.io")),
  licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT")),
  description := "ErgoPad random token sale and mega minting protocol SDK.",
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/ergo-pad/random-token-sale-sdk"),
      "scm:git@github.com:ergo-pad/random-token-sale-sdk.git"
    )
  ),
  developers := List(
    Developer(
      "lgd",
      "Luca D'Angelo",
      "ldgaetano@protonmail.com",
      url("https://github.com/lucagdangelo")
    )
  ),
  
  sonatypeCredentialHost := "s01.oss.sonatype.org",
  sonatypeRepository := "https://s01.oss.sonatype.org/service/local",

  scalaVersion := "2.12.15",
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.2.14",
    "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    "org.ergoplatform" %% "ergo-appkit" % "4.0.10",
    "com.google.code.gson" % "gson" % "2.8.5",
  ),

  versionScheme := Some("semver-spec"),
  assembly / assemblyJarName := s"${name.value}-${version.value}.jar",
  assembly / assemblyOutputPath := file(s"./${name.value}-${version.value}.jar/")
  
))