lazy val commonsCollectionsVersion = "4.01"
lazy val commonsLangVersion = "3.0.1"
lazy val querydslVersion = "2.3.0"

libraryDependencies ++= Seq(
  "com.github.javaparser" % "javaparser-core" % "2.5.1",
  "net.sourceforge.collections" % "collections-generic" % commonsCollectionsVersion,
  "org.apache.commons" % "commons-lang3" % commonsLangVersion,
  "org.apache.commons" % "commons-io" % "1.3.2",
  "org.scalameta" %% "scalafmt-core" % "2.6.3",

  "org.scala-lang" % "scala-library" % scalaVersion.value % Test,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,

  "com.novocode" % "junit-interface" % "0.11" % Test,

  "junit" % "junit" % "4.8.1" % Test exclude("javax.servlet", "servlet-api"),
  "com.mysema.querydsl" % "querydsl-core" % querydslVersion % Test,
  "com.mysema.commons" % "mysema-commons-lang" % "0.2.4" % Test,
  "com.google.code.findbugs" % "jsr305" % "3.0.1" % Test,
  "com.mysema.codegen" % "codegen" % "0.6.8" % Test,
  "javax.inject" % "javax.inject" % "1" % Test,
  "com.jsuereth" %% "scala-arm" % "2.0" % Test
)

//fork in test := true
//baseDirectory in test := baseDirectory(_ / "scalagen").value
