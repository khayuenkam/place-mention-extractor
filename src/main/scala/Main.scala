import java.io.{File, FileWriter}

import WikiTextHelper.{AnnotateOption, SentenceOption}
import doobie.implicits._
import info.bliki.wiki.filter.PlainTextConverter
import info.bliki.wiki.model.WikiModel

import scala.collection.immutable.Queue

object Main extends App {
  case class PageLink(title: String, pageId: String)
  val excludeTitles = Set("列表", "事件")
  val tag = "PLACE"
  val maxTokens = 50

  def linkTitle(sortKey: String, prefix: String): String = {
    sortKey.replace(prefix, "").replaceAll("\\s+", " ").trim
  }
  def pageTitle(title: String): String = {
    title.replaceAll("_", " ").replaceAll("\\s+", " ").trim
  }
  def excluded(title: String): Boolean = {
    excludeTitles.exists(title.contains(_))
  }
  def validTitle(title: String): Boolean = {
    title.nonEmpty && !excluded(title)
  }

  def getLabelsAndPageQueue(categoryLinks: List[String]): (Map[String, String], Queue[PageLink]) = {
    def loop(list: List[String], labels: Map[String, String],
             queue: Queue[PageLink], processedKeys: Set[String]): (Map[String, String], Queue[PageLink]) = {
      if (list.isEmpty) (labels, queue)
      else {
        val categoryLinks = Database.getCategoryLinks(list).transact(Database.xa).unsafeRunSync()
        val newProcessedKeys = processedKeys ++ list
        val pageCategoryLinks = categoryLinks.par.filter(c => c.linkType == "page")

        val (newPageLabels, newQueue) = pageCategoryLinks
          .map(page => (pageTitle(page.pageTitle.getOrElse("")), page.from))
          .filter(x => validTitle(x._1))
          .toList
          .foldLeft((labels, queue))((acc, x) => (acc._1 + (x._1 -> tag), acc._2.enqueue(PageLink(x._1, x._2))))

        val pageTitles = pageCategoryLinks.filter(_.pageTitle.isDefined).map(_.pageTitle.get)
        val redirectedTitles = if (pageTitles.nonEmpty)
          Database.getRedirectedTitles(pageTitles.toList).transact(Database.xa).unsafeRunSync()
        else List.empty[String]

        val newLabels = redirectedTitles.foldLeft(newPageLabels)((labels, title) => labels + (title -> tag))
        println(s"Found ${redirectedTitles.length} redirected titles.")

        val categoryKeys = categoryLinks
          .filter(c => c.linkType == "subcat" && !newProcessedKeys.contains(c.sortKey))
          .map(c => c.sortKey)

        loop(categoryKeys, newLabels, newQueue, newProcessedKeys)
      }
    }

    loop(categoryLinks, Map.empty[String, String], Queue[PageLink](), Set[String]())
  }

  def processQueueAndWriteToFile(labels: Map[String, String], queue: Queue[PageLink]): Unit = {
    val wikiModel = new WikiModel("wiki/${image}", "wiki/${title}")
    val sortedEntries = labels.keys.toArray.sortWith(_.length > _.length)
    val sentenceOption = SentenceOption("。", "", maxTokens)
    val outputWriter = new FileWriter(new File("annotations.txt"))
    var count = 0

    def loop(queue: Queue[PageLink]): Unit = {
      if (queue.nonEmpty) {
        val (link, restQueue) = queue.dequeue
        val source = Database.getPageSource(link.pageId).transact(Database.xa).unsafeRunSync()
        if (source.nonEmpty) {
          val text = wikiModel.render(new PlainTextConverter(), source.get)
          val sentences = WikiTextHelper.sentencesContains(text, link.title, sentenceOption)
          val annotations = sentences.map(WikiTextHelper.annotate(_, sortedEntries, labels, AnnotateOption("")))
          annotations.foreach({ s => outputWriter.write(s"$s\n") })
          count += annotations.size
          println(s"Total annotations: $count")
        }
        loop(restQueue)
      } else {
        outputWriter.close()
      }
    }

    loop(queue)
  }

  val (labels, queue) = getLabelsAndPageQueue(List("各大洲旅遊景點"))

  processQueueAndWriteToFile(labels, queue)
}
