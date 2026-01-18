name := "lila-ws"

version := "2.2"

lazy val `lila-ws` = (project in file("."))
  .enablePlugins(JavaAppPackaging)

val akkaVersion          = "2.6.18"
val kamonVersion         = "2.8.0"
val nettyVersion         = "4.1.63.Final"
val reactivemongoVersion = "1.1.0-RC19"

scalaVersion := "3.7.4"

libraryDependencies += "org.reactivemongo"          %% "reactivemongo"                % reactivemongoVersion
libraryDependencies += "org.reactivemongo"          %% "reactivemongo-bson-api"       % reactivemongoVersion

excludeDependencies += "org.scala-lang.modules" % "scala-java8-compat_2.13"
libraryDependencies += "org.reactivemongo"           % "reactivemongo-shaded-native-linux-x86-64"  % reactivemongoVersion
libraryDependencies += "io.lettuce"                  % "lettuce-core"                 % "6.1.2.RELEASE"
libraryDependencies += "io.netty"                    % "netty-handler"                % nettyVersion
libraryDependencies += "io.netty"                    % "netty-codec-http"             % nettyVersion
libraryDependencies += "io.netty"                    % "netty-transport-native-epoll" % nettyVersion classifier "linux-x86_64"
libraryDependencies += "org.playstrategy"           %% "strategygames"                % "10.2.1-pstrat209.lw20260417.1"
libraryDependencies += "com.typesafe.akka"          %% "akka-actor-typed"             % akkaVersion
//libraryDependencies += "com.typesafe.akka"          %% "akka-slf4j"                   % akkaVersion
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging"                % "3.9.6"
libraryDependencies += "joda-time"                   % "joda-time"                    % "2.10.10"
libraryDependencies += "com.github.blemale"         %% "scaffeine"                    % "5.3.0" % "compile"
libraryDependencies += "ch.qos.logback"              % "logback-classic"              % "1.5.20"
libraryDependencies += "com.typesafe.play"          %% "play-json"                    % "2.10.8"
libraryDependencies += "io.kamon"                   %% "kamon-core"                   % kamonVersion
libraryDependencies += "io.kamon"                   %% "kamon-influxdb"               % kamonVersion
libraryDependencies += "io.kamon"                   %% "kamon-system-metrics"         % kamonVersion
libraryDependencies += "com.softwaremill.macwire"   %% "macros"                       % "2.6.7" % "provided"
libraryDependencies += "com.outr"                   %% "hasher"                       % "1.2.3"

resolvers += Resolver.sonatypeCentralSnapshots
resolvers += "lila-maven" at "https://raw.githubusercontent.com/Mind-Sports-Games/lila-maven/master"

resolvers ++= Seq(
  "lila-maven".at("https://raw.githubusercontent.com/Mind-Sports-Games/lila-maven/master"),
  "jitpack".at("https://jitpack.io")
) ++ sys.env
  .get("LILA_MAVEN_RESOLVERS")
  .map(_.split(",").zipWithIndex.map { case (x, i) => s"local-maven-$i" at x })
  .map(_.toSeq)
  .getOrElse(Seq())

scalacOptions ++= Seq(
  "-encoding",
  "utf-8",
  "-rewrite",
  "-source:3.7",
  "-explaintypes",
  "-feature",
  "-Xtarget:21",
  "-Wunused:all",
  "-explain",
  "-language:implicitConversions",
)

javaOptions ++= Seq("-Xms32m", "-Xmx128m")

Compile / doc / sources := Seq.empty

Compile / packageDoc / publishArtifact := false

/* scalafmtOnCompile := true */


