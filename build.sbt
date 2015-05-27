name := "proto-proxy"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= {
  val akkaV       = "2.3.11"
  val akkaStreamV = "1.0-RC3"
  val playV       = "2.4.0-RC5"
  Seq(
    "com.typesafe.akka" %% "akka-actor"                        % akkaV,
    "com.typesafe.akka" %% "akka-slf4j" % akkaV,
    "com.typesafe.akka" %% "akka-stream-experimental"          % akkaStreamV,
    "com.typesafe.akka" %% "akka-http-experimental"      % akkaStreamV,
    "com.typesafe.play" %% "play-json"                         % playV,
    "org.slf4j" % "slf4j-api" % "1.7.9",
    "org.apache.logging.log4j" % "log4j-to-slf4j" % "2.0.2",
    "org.specs2" %% "specs2-core" % "3.6" % "test"
  )
}