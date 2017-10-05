package provingground.translation
import provingground._

import edu.stanford.nlp.ling._
import edu.stanford.nlp.process._

import edu.stanford.nlp.trees._

import edu.stanford.nlp.parser.lexparser._
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.tagger.maxent._
import java.io._
import scala.collection.JavaConverters._

/**
  * Interface to the Stanford parser, handling (inline) TeX by separating tokenizing and POS tagging from parsing.
  * Parsing is done by the [[texParse]] method
  */
object StanfordParser {
  val lp = LexicalizedParser.loadModel(
    "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")

  lazy val tlp = new PennTreebankLanguagePack

  lazy val gsf = tlp.grammaticalStructureFactory

  val tagger = new MaxentTagger(
    "edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger")

  val tokenizerFactory = PTBTokenizer.factory(new CoreLabelTokenFactory(), "")

  def coreLabels(s: String) =
    tokenizerFactory.getTokenizer(new StringReader(s)).tokenize

  def words(s: String) = coreLabels(s).asScala map ((c) => new Word(c.word))

  def parse(s: String) = lp(tagger(words(s).asJava))

  def texInline(s: String) = """\$[^\$]+\$""".r.findAllIn(s)

  def texDisplay(s: String) = """\$\$[^\$]+\$\$""".r.findAllIn(s)

  def reTag(word: String, tag: String)(tw: TaggedWord) =
    if (tw.word.toLowerCase == word) new TaggedWord(tw.word, tag) else tw

  def mergeTag(mwe: Vector[String], tag: String)(
      tws: Vector[TaggedWord]): Vector[TaggedWord] =
    if (tws.take(mwe.size).map(_.word.toLowerCase) == mwe)
      (new TaggedWord(tws.take(mwe.size).map(_.word).mkString(" "), tag)) +: tws
        .drop(mwe.size)
    else
      tws match {
        case Vector() => Vector()
        case x +: ys  => x +: mergeTag(mwe, tag)(ys)
      }

  def mergeSubs(mwe: Vector[String], tw: TaggedWord)(
      tws: Vector[TaggedWord]): Vector[TaggedWord] =
    if (tws.take(mwe.size).map(_.word.toLowerCase) == mwe)
      tw +: tws.drop(mwe.size)
    else
      tws match {
        case Vector() => Vector()
        case x +: ys  => x +: mergeSubs(mwe, tw)(ys)
      }

  case class TeXParsed(
      preraw: String,
      wordTags: Vector[(String, String)] = Vector(),
      mweSubs: Vector[(Vector[String], TaggedWord)] = Vector()
      // , mweTags: Vector[(Vector[String], String)] = Vector()
  ) {
    val raw = preraw
      .replace("such that", "so that")
    // .replace("which", "where it") // useful for contituency parsing
    // .replace("that", "where it")

    lazy val texMap = (texInline(raw).zipWithIndex map {
      case (w, n) => (s"TeXInline$n", w)
    }).toMap

    lazy val deTeXed = (texMap :\ raw) { case ((l, w), s) => s.replace(w, l) }

    lazy val deTeXWords = words(deTeXed)

    lazy val deTeXTagged = tagger(deTeXWords.asJava)

    def reTagged(tw: TaggedWord) =
      wordTags.foldRight(tw) { case ((w, tag), t) => reTag(w, tag)(t) }

    lazy val tagged =
      deTeXTagged.asScala map { (tw) =>
        if (tw.word.startsWith("TeXInline"))
          new TaggedWord(texMap(tw.word), "NNP")
        else reTagged(tw)
      }

    lazy val mergeSubsTagged =
      mweSubs.foldRight(tagged.toVector) {
        case ((ws, tw), t) => mergeSubs(ws, tw)(t)
      }

    // lazy val mergeTagged =
    //     mweTags.foldRight(tagged.toVector){case ((ws, tag), t) => mergeTag(ws, tag)(t)}

    lazy val parsed = lp(mergeSubsTagged.asJava)

    lazy val gs = gsf.newGrammaticalStructure(parsed)

    lazy val tdl = gs.typedDependenciesCCprocessed

    import translation.NlpProse._

    def token(w: IndexedWord) = Token(w.word, w.index)

    lazy val typedDeps =
      tdl.asScala.map { (x) =>
        DepRel(token(x.gov), token(x.dep), x.reln.toString)
      }

    lazy val proseTree = ProseTree(typedDeps.toList)
  }

  val baseWordTags =
    Vector("iff" -> "CC", "modulo" -> "IN")

  val baseMweSubs =
    Vector(Vector("if", "and", "only", "if") -> new TaggedWord("iff", "CC"),
           Vector("such", "that")            -> new TaggedWord("where, WRB"))

  def texParse(s: String,
               wordTags: Vector[(String, String)] = baseWordTags,
               // mweTags: Vector[(Vector[String], String)] = Vector(),
               mweSubs: Vector[(Vector[String], TaggedWord)] = baseMweSubs) =
    TeXParsed(s, wordTags, mweSubs).parsed

  def proseTree(s: String,
                wordTags: Vector[(String, String)] = baseWordTags,
                // mweTags: Vector[(Vector[String], String)] = Vector(),
                mweSubs: Vector[(Vector[String], TaggedWord)] = baseMweSubs) =
    TeXParsed(s, wordTags, mweSubs).proseTree

}
