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
import java.util

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.matching.Regex

import interface.WordNet

/**
  * Interface to the Stanford parser, handling (inline) TeX by separating tokenizing and POS tagging from parsing.
  * Parsing is done by the [[texParse]] method
  */
object StanfordParser {
  val lp: LexicalizedParser = LexicalizedParser.loadModel(
    "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")

  lazy val tlp: PennTreebankLanguagePack = new PennTreebankLanguagePack

  lazy val gsf: GrammaticalStructureFactory = tlp.grammaticalStructureFactory

  val tagger: MaxentTagger = new MaxentTagger(
    "edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger")

  val tokenizerFactory: TokenizerFactory[CoreLabel] = PTBTokenizer.factory(new CoreLabelTokenFactory(), "")

  def coreLabels(s: String): util.List[CoreLabel] =
    tokenizerFactory.getTokenizer(new StringReader(s)).tokenize

  def words(s: String): mutable.Buffer[Word] = coreLabels(s).asScala map ((c) => new Word(c.word))

  def parse(s: String): Tree = lp(tagger(words(s).asJava))

  def texInline(s: String): Regex.MatchIterator = """\$[^\$]+\$""".r.findAllIn(s)

  def texDisplay(s: String): Regex.MatchIterator = """\$\$[^\$]+\$\$""".r.findAllIn(s)

  def reTag(word: String, tag: String)(tw: TaggedWord): TaggedWord =
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

  def wordNetTags(tws: Vector[TaggedWord]) : Vector[Vector[TaggedWord]] =
    tws match {
      case Vector() => Vector(Vector())
      case x +: ys =>
        val tags = x.tag +: WordNet.posTags(x.word)
        pprint.log(tags)
        for {
          tag <- tags
          tail <- wordNetTags(ys)
          tw = new TaggedWord(x.word, tag)
          // _ = pprint.log(tw)
          // _ = pprint.log(tail)
        } yield tw +: tail
    }

  case class TeXParsed(
      preraw: String,
      wordTags: Vector[(String, String)] = baseWordTags,
      mweSubs: Vector[(Vector[String], TaggedWord)] = baseMweSubs
      // , mweTags: Vector[(Vector[String], String)] = Vector()
  ) {
    val raw: String = preraw
      // .replace("such that", "so that")
    // .replace("which", "where it") // useful for contituency parsing
    // .replace("that", "where it")

    lazy val texMap: Map[String, String] = (texInline(raw).zipWithIndex map {
      case (w, n) => (s"TeXInline$n", w)
    }).toMap

    lazy val deTeXed: String = (texMap :\ raw) { case ((l, w), s) => s.replace(w, l) }

    lazy val deTeXWords: mutable.Buffer[Word] = words(deTeXed)

    lazy val deTeXTagged: util.List[TaggedWord] = tagger(deTeXWords.asJava)

    def reTagged(tw: TaggedWord): TaggedWord =
      wordTags.foldRight(tw) { case ((w, tag), t) => reTag(w, tag)(t) }

    lazy val tagged: mutable.Buffer[TaggedWord] =
      deTeXTagged.asScala map { (tw) =>
        if (tw.word.startsWith("TeXInline"))
          new TaggedWord(texMap(tw.word), "NNP")
        else reTagged(tw)
      }

    lazy val mergeSubsTagged: Vector[TaggedWord] =
      mweSubs.foldRight(tagged.toVector) {
        case ((ws, tw), t) => mergeSubs(ws, tw)(t)
      }

    // lazy val mergeTagged =
    //     mweTags.foldRight(tagged.toVector){case ((ws, tag), t) => mergeTag(ws, tag)(t)}

    lazy val parsed: Tree = lp(mergeSubsTagged.asJava)

    lazy val polyParsed: Vector[Tree] =
      for {
        tws <- wordNetTags(mergeSubsTagged)
      } yield lp(tws.asJava)

    lazy val gs: GrammaticalStructure = gsf.newGrammaticalStructure(parsed)

    lazy val tdl: util.List[TypedDependency] = gs.typedDependenciesCCprocessed

    import translation.NlpProse._

    def token(w: IndexedWord) = Token(w.word, w.index)

    lazy val typedDeps: mutable.Buffer[DepRel] =
      tdl.asScala.map { (x) =>
        DepRel(token(x.gov), token(x.dep), x.reln.toString)
      }

    lazy val proseTree: ProseTree = ProseTree(typedDeps.toList)
  }

  val baseWordTags =
    Vector("iff" -> "IN", "modulo" -> "IN")

  val baseMweSubs =
    Vector(Vector("if", "and", "only", "if") -> new TaggedWord("iff", "IN"),
           Vector("such", "that")            -> new TaggedWord("with", "IN"))

  def texParse(s: String,
               wordTags: Vector[(String, String)] = baseWordTags,
               // mweTags: Vector[(Vector[String], String)] = Vector(),
               mweSubs: Vector[(Vector[String], TaggedWord)] = baseMweSubs): Tree =
    TeXParsed(s, wordTags, mweSubs).parsed

  def proseTree(s: String,
                wordTags: Vector[(String, String)] = baseWordTags,
                // mweTags: Vector[(Vector[String], String)] = Vector(),
                mweSubs: Vector[(Vector[String], TaggedWord)] = baseMweSubs)
    : NlpProse.ProseTree =
    TeXParsed(s, wordTags, mweSubs).proseTree

}
