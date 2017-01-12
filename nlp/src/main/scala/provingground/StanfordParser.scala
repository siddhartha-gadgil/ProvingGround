package provingground

// import edu.stanford.nlp.io._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.process._
// import edu.stanford.nlp.trees._
// import edu.stanford.nlp.util._
// import edu.stanford.nlp.ling.CoreAnnotations._
// import edu.stanford.nlp.semgraph._
// import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations._
import edu.stanford.nlp.parser.lexparser._
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.tagger.maxent._
import java.io._
import scala.collection.JavaConversions._

object StanfordParser{
  val lp = LexicalizedParser.loadModel("edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")

  val tagger = new MaxentTagger("edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger")

  val tokenizerFactory = PTBTokenizer.factory(new CoreLabelTokenFactory(), "")

  def coreLabels(s: String) = tokenizerFactory.getTokenizer(new StringReader(s)).tokenize

  def words(s: String) = coreLabels(s) map ((c) => new Word(c.word))

  def parse(s: String) = lp(tagger(words(s)))

  def texInline(s: String) = """\$[^\$]+\$""".r.findAllIn(s)

  def texDisplay(s: String) = """\$\$[^\$]+\$\$""".r.findAllIn(s)

  case class TeXParsed(raw: String){
    lazy val texMap = (texInline(raw).zipWithIndex map {case (w, n) => (s"TeXInline$n", w)}).toMap

    lazy val deTeXed = (texMap :\ raw){case ((l, w), s) => s.replace(w, l)}

    lazy val deTeXWords = words(deTeXed)

    lazy val deTeXTagged = tagger(deTeXWords)

    lazy val tagged = deTeXTagged map {
      (tw) =>
      if (tw.word.startsWith("TeXInline"))  new TaggedWord(texMap(tw.word), "NNP") else tw}

    lazy val parsed = lp(tagged)
  }

  def texParse(s: String) = TeXParsed(s).parsed
}
