name := """ems_platform"""
organization := "com.asu"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)
  .disablePlugins(PlayFilters)

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
  "org.webjars" %% "webjars-play" % "2.6.1",
  "com.typesafe.play" %% "play-json" % "2.6.0",
  "org.webjars" % "bootstrap" % "4.1.1",
  "org.webjars" % "font-awesome" % "5.0.13",
  "org.webjars" % "jquery" % "3.3.1-1",
 "org.webjars.npm" % "popper.js" % "1.14.3",
  "org.webjars" % "dropzone" % "5.2.0",
  //"org.webjars.npm" % "jqtree" % "1.4.2" exclude("org.webjars.npm", "jquery"),


  "org.webjars.bower" % "datatables" % "1.10.18",

  "org.webjars.npm" % "bootstrap-colorpicker" % "2.5.2" exclude("org.webjars.npm", "jquery"),
  "org.webjars.bower" % "datatables-responsive" % "2.2.1" exclude("org.webjars.bower","datatables"),
  "org.webjars.bower" % "cytoscape" % "3.2.5",
  "org.webjars.npm" % "cytoscape-panzoom" % "2.4.0",
  "org.webjars.npm" % "github-com-iVis-at-Bilkent-cytoscape-js-context-menus" % "2.0.3",
  "org.webjars.npm" % "cytoscape-cose-bilkent" % "2.1.0"
)

routesGenerator := InjectedRoutesGenerator

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.asu.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.asu.binders._"
