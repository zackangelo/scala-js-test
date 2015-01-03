import scala.scalajs.sbtplugin.ScalaJSPlugin._
import com.lihaoyi.workbench.Plugin._

scalaJSSettings

workbenchSettings

name := "Example"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.2"

resolvers += "bintray-alexander_myltsev" at "http://dl.bintray.com/content/alexander-myltsev/maven"

libraryDependencies ++= Seq(
  "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6",
  "com.scalatags" %%% "scalatags" % "0.4.2",
  "name.myltsev" %% "shapeless_sjs0.5" % "2.0.0"
)

bootSnippet := "AppRouter().startRouting();"

updateBrowsers <<= updateBrowsers.triggeredBy(ScalaJSKeys.fastOptJS in Compile)

