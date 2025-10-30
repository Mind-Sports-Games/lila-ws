name := "lila-ws"

version := "2.2"

lazy val `lila-ws` = (project in file("."))
  .enablePlugins(JavaAppPackaging)

val akkaVersion          = "2.6.14"
val kamonVersion         = "2.1.18"
val nettyVersion         = "4.1.63.Final"
val reactivemongoVersion = "1.1.0-RC15"

scalaVersion := "2.13.9"

libraryDependencies += "org.reactivemongo"          %% "reactivemongo"                % reactivemongoVersion exclude ("org.scala-lang.modules", "scala-java8-compat_2.13")
libraryDependencies += "org.reactivemongo"          %% "reactivemongo-bson-api"       % reactivemongoVersion
libraryDependencies += "org.reactivemongo"           % "reactivemongo-shaded-native-linux-x86-64"  % reactivemongoVersion
libraryDependencies += "io.lettuce"                  % "lettuce-core"                 % "6.1.2.RELEASE"
libraryDependencies += "io.netty"                    % "netty-handler"                % nettyVersion
libraryDependencies += "io.netty"                    % "netty-codec-http"             % nettyVersion
libraryDependencies += "io.netty"                    % "netty-transport-native-epoll" % nettyVersion classifier "linux-x86_64"
libraryDependencies += "org.playstrategy"           %% "strategygames"                % "10.2.1-pstrat188"
libraryDependencies += "com.typesafe.akka"          %% "akka-actor-typed"             % akkaVersion
//libraryDependencies += "com.typesafe.akka"          %% "akka-slf4j"                   % akkaVersion
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging"                % "3.9.6"
libraryDependencies += "joda-time"                   % "joda-time"                    % "2.10.10"
libraryDependencies += "com.github.blemale"         %% "scaffeine"                    % "4.0.2" % "compile"
libraryDependencies += "ch.qos.logback"              % "logback-classic"              % "1.5.20"
libraryDependencies += "com.typesafe.play"          %% "play-json"                    % "2.9.2"
libraryDependencies += "io.kamon"                   %% "kamon-core"                   % kamonVersion
libraryDependencies += "io.kamon"                   %% "kamon-influxdb"               % kamonVersion
libraryDependencies += "io.kamon"                   %% "kamon-system-metrics"         % kamonVersion
libraryDependencies += "com.softwaremill.macwire"   %% "macros"                       % "2.3.7" % "provided"
libraryDependencies += "com.roundeights"            %% "hasher"                       % "1.2.1"

resolvers ++= Resolver.sonatypeOssRepos("snapshots")
resolvers += "lila-maven" at "https://raw.githubusercontent.com/Mind-Sports-Games/lila-maven/master"

val localMaven = sys.env
  .get("LILA_MAVEN_RESOLVERS")
  .map(_.split(",").zipWithIndex.map { case (x, i) => s"local-maven-$i" at x })
  .map(_.toSeq)
  .getOrElse(Seq())

resolvers ++= localMaven

scalacOptions ++= Seq(
  "-explaintypes",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-Ymacro-annotations",
  // Warnings as errors!
  // "-Xfatal-warnings",
  // Linting options
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:deprecation",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wnumeric-widen",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wunused:implicits",
  "-Wunused:params"
  /* "-Wvalue-discard" */
)

javaOptions ++= Seq("-Xms32m", "-Xmx128m")

Compile / doc / sources := Seq.empty

Compile / packageDoc / publishArtifact := false

/* scalafmtOnCompile := true */
