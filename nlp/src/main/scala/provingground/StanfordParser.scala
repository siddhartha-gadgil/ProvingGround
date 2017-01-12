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
}
