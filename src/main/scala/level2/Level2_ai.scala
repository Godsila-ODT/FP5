package level2

import scala.xml.XML
import java.nio.file.{Files, Paths}
import scala.io.Source

// A Unary Function — Effect: read the raw XML from disk
def fetchData(xmlPath: String): String =
  val source = Source.fromFile(xmlPath)
  try source.mkString
  finally source.close()

// A Unary Function — Pure Logic: parse XML into a List of (title, link)
def parseRSS(xmlContent: String): List[(String, String)] =
  val xml = XML.loadString(xmlContent)
  (xml \\ "item").toList.map: node =>
    val title = (node \ "title").text.trim
    val link = (node \ "link").text.trim
    (title, link)

// Pure Logic: render the items into a report string
def formatter(items: List[(String, String)]): String =
  items
    .map((title, link) => s"Title: $title\nLink: $link\n---\n")
    .mkString

// Effect: write the report to disk
def saveToFile(path: String, content: String): Unit =
  Files.write(Paths.get(path), content.getBytes)

// Orchestration: compose the pieces. fetch -> parse -> format
def processFeed(
    fetch: String => String, // Higher-Order Parameter
    parse: String => List[(String, String)], // Higher-Order P,arameter
    formatter: List[(String, String)] => String // Higher-Order Parameter
    // Save to file
)(xmlPath: String): String =
  formatter(parse(fetch(xmlPath)))

@main def runLevel2(): Unit =
  val bbcFeeds = List(
    "sample-data/level4/tech.xml" -> "output/tech_news.txt",
    "sample-data/level4/business.xml" -> "output/business_news.txt"
  )

  val rssProcessed = processFeed(fetchData, parseRSS, formatter)

  bbcFeeds.foreach: (xmlPath, path) =>
    println(s"  [Start] Processing $xmlPath")
    val content: String = rssProcessed(xmlPath)
    saveToFile(path, content)
    println(s"  [Done] Saved to $path")
