name := "jesc"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "com.hp.hpl.jena" %  "jena"           % "2.6.4",
  "org.specs2"      %% "specs2-core"    % "3.3.1" % "test",
  "org.scalatest"   %  "scalatest_2.11" % "2.2.4" % "test")
