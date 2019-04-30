name := "zupon_andrew_project"

version := "1.0-SNAPSHOT"

organization := "edu.arizona.cs"

scalaVersion := "2.12.6"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.clulab" %% "processors-main" % "7.4.3",
  "org.clulab" %% "processors-corenlp" % "7.4.3",
  "org.clulab" %% "processors-modelscorenlp" % "7.4.3",
  "org.clulab" %% "processors-modelsmain" % "7.4.3",
  "org.apache.lucene" % "lucene-core" % "7.6.0",
  "org.apache.lucene" % "lucene-queryparser" % "7.6.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "7.6.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
