name := "place-mention-extractor"
version := "1.0.0"
scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "info.bliki.wiki" % "bliki-core" % "3.1.0",
  "org.tpolecat" %% "doobie-core" % "0.5.0-M7",
  "org.mariadb.jdbc" % "mariadb-java-client" % "2.1.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)
