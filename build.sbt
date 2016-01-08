enablePlugins(ScalaJSPlugin)

name := "GCHQ 2015 Christmas Puzzle"

scalaVersion := "2.11.7"

scalaJSStage in Global := FastOptStage

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.2"
libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.1"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.2"

skip in packageJSDependencies := false

persistLauncher in Compile := true
