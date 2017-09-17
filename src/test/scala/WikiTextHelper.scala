import WikiTextHelper.{AnnotateOption, SentenceOption, TextData}
import org.scalatest._

class WikiTextHelperTest extends FunSuite {
  val paragraph: String =
    """
      |The corpora comprise files divided by language, encoded in the BIO format (Ramshaw & Marcus, 1995).
      |The BIO format is a simple, text-based format that divides texts into single tokens per line, and, separated by a whitespace, tags to indicate which ones are named entities.
      |The most commonly used tags are PER (person), LOC (location) and ORG (organization). To indicate named entities that span multiple tokens,
      |the tags have a prefix of either B- (begining of named entity) or I- (continuation of named entity). O tags are used to indicate that the token is not a named entity.
    """.stripMargin

  test("WikiTextHelper should return sentences that contain entity") {
    val sentences = WikiTextHelper.sentencesContains(paragraph, "BIO format", SentenceOption(".", " ", 25))
    val expected = Array(
      "The corpora comprise files divided by language, encoded in the BIO format (Ramshaw & Marcus, 1995).",
      "The BIO format is a simple, text-based format that divides texts into single tokens per line, and, separated by a whitespace, tags to indicate which."
    )
    assert(sentences.sameElements(expected))
  }

  test("WikiTextHelper should split links") {
    val text = "The [[quick brown fox|firefox]] jumps over the [[lazy dog]]."
    val splits = WikiTextHelper.splitLinks(text, AnnotateOption(" "))
    val expected = Array(
      "The ", "[[quick brown fox|firefox]]", " jumps over the ", "[[lazy dog]]", "."
    )
    assert(splits.sameElements(expected))
  }

  test("WikiTextHelper should return TextData array") {
    val text = "The [[quick brown fox|firefox]] jumps over the [[lazy dog]]."
    val textData = WikiTextHelper.textData(text, AnnotateOption(" "))
    val expected = Array(
      TextData("The", Option.empty, isLink = false),
      TextData("quick brown fox", Option("firefox"), isLink = true),
      TextData("jumps over the", Option.empty, isLink = false),
      TextData("lazy dog", Option.empty, isLink = true),
      TextData(".", Option.empty, isLink = false)
    )
    assert(textData.sameElements(expected))
  }

  test("WikiTextHelper should split entity") {
    val entities = Array("quick brown fox", "lazy dog", "faith", "head with a brick", "life")
    val text = "The quick brown fox jumps over the lazy dog. Sometimes life is going to hit you in the head with a brick. Don't lose faith. "
    val splits = WikiTextHelper.splitEntity(text, entities)
    val expected = Array(
      ("The ", false),
      ("quick brown fox", true),
      (" jumps over the ", false),
      ("lazy dog",true),
      (". Sometimes ", false),
      ("life", true),
      (" is going to hit you in the ", false),
      ("head with a brick", true),
      (". Don't lose ", false),
      ("faith", true),
      (". ", false)
    )
    assert(splits.sameElements(expected))
  }

  test("WikiTextHelper should return annotations") {
    val labels = Map(
      "firefox" -> "ORG",
      "lazy dog" -> "PER",
      "faith" -> "MISC",
      "head with a brick" -> "PER",
      "life" -> "ORG"
    )
    val entries = labels.keys.toArray.sortWith(_.length > _.length)
    val text = "The [[quick brown fox|firefox]] jumps over the [[lazy dog]]. Sometimes life is going to hit you in the head with a brick. Don't lose faith. "
    val annotated = WikiTextHelper.annotate(text, entries, labels, AnnotateOption(" "))
    val expected = "The/O quick/B-ORG brown/I-ORG fox/I-ORG jumps/O over/O the/O lazy/B-PER dog/I-PER ./O Sometimes/O life/B-ORG is/O going/O to/O hit/O you/O in/O the/O head/B-PER with/I-PER a/I-PER brick/I-PER ./O Don't/O lose/O faith/B-MISC ./O"
    assert(annotated.equals(expected))
  }
}