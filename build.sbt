organization in ThisBuild := "com.mysema.scalagen"

version in ThisBuild := "0.3.2"

name := "scalagen-root"

lazy val sclVersions = List("2.12.11")

scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation")

scalaVersion in ThisBuild := sclVersions.head

lazy val scalagen = project
