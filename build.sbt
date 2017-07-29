name := """fp_practice"""

version := "1.0"

scalaVersion := "2.12.1"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8"

libraryDependencies += "org.typelevel" %% "cats" % "0.8.1"

libraryDependencies += "com.typesafe" % "config" % "1.3.1"

libraryDependencies +=  "com.chuusai" %% "shapeless" % "2.3.2"

libraryDependencies += "org.spire-math" %% "jawn-ast" % "0.11.0"