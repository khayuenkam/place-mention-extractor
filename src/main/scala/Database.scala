import cats.effect.IO
import cats.implicits._
import doobie._
import Fragments.{in, whereAndOpt}
import doobie.implicits._
import doobie.util.transactor.Transactor
import models.CategoryLink

object Database {
  private lazy val host = "127.0.0.1"
  private lazy val port = 3306
  private lazy val db = "zhwiki"
  val xa = Transactor.fromDriverManager[IO](
    "org.mariadb.jdbc.Driver", s"jdbc:mariadb://$host:$port/$db",
    "dataUser", "dataUserPassword"
  )

  def getCategoryLinks(categories: List[String]): ConnectionIO[List[CategoryLink]] = {
    val categoriesIn = categories.toNel.map(c => in(fr"cl_to".asInstanceOf[Fragment], c))

    val query = fr"""
      SELECT cl_from, cl_to, cl_sortkey, cl_sortkey_prefix, cl_type, page_title
      FROM categorylinks
      LEFT JOIN page ON page_id = cl_from
      """.asInstanceOf[Fragment] ++ whereAndOpt(categoriesIn)

    query.query[CategoryLink].list
  }

  def getPageSource(pageId: String): ConnectionIO[Option[String]] = {
    val query = sql"""
      SELECT old_text
      FROM page p
      LEFT JOIN text t ON t.old_id = p.page_latest
      WHERE page_id = $pageId
    """.asInstanceOf[Fragment]

    query.query[String].option
  }

  def getRedirectedTitles(pageIds: List[String]): ConnectionIO[List[String]] = {
    val pageIdsIn = pageIds.toNel.map(pI => in(fr"rd_title".asInstanceOf[Fragment], pI))

    val query = fr"""
      SELECT page_title
      FROM redirect r
      LEFT JOIN page p ON p.page_id = r.rd_from
      """.asInstanceOf[Fragment] ++ whereAndOpt(pageIdsIn)

    query.query[String].list
  }
}
