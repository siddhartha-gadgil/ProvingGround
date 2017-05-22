package provingground

import translation._

import NlpProse._
//import provingground.ParseProse._

import java.io._
// import java.util.*;
import scala.collection.JavaConverters._

import edu.stanford.nlp.io._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.trees._
import edu.stanford.nlp.util._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.semgraph._
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations._

object CoreNLP {

  def gov(e: SemanticGraphEdge) =
    Token(e.getGovernor().word, e.getGovernor().index)

  def dep(e: SemanticGraphEdge) =
    Token(e.getDependent().word, e.getDependent().index)

  def depWord(short: String, specific: String) =
    if (short == "prep") short + "_" + specific else short

  def depType(e: SemanticGraphEdge) =
    depWord(e.getRelation().getShortName(), e.getRelation.getSpecific())

  def newPipe = {
    val props = new java.util.Properties()
    props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
    new StanfordCoreNLP(props)
  }

  implicit lazy val pipe = newPipe

  def annotatedDoc(text: String, pipe: StanfordCoreNLP) = {
    val document = new Annotation(text)
    pipe.annotate(document)
    document
  }

  def sentences(document: Annotation): List[CoreMap] = {
    (document.get(classOf[SentencesAnnotation])).asScala.toList
  }

  def depRelIterable(sentence: CoreMap) = {
    val dependencies =
      sentence.get(classOf[CollapsedCCProcessedDependenciesAnnotation]);
    val dependencyIterable = dependencies.edgeIterable().asScala
    dependencyIterable map
      ((e: SemanticGraphEdge) => DepRel(gov(e), dep(e), depType(e)))
  }

  def proseTrees(text: String)(implicit pipe: StanfordCoreNLP) = {
    for (sentence <- sentences(annotatedDoc(text, pipe))) yield {
      new ProseTree(depRelIterable(sentence).toList)
    }
  }

  def coreLabelList(sentence: CoreMap) = {
    (sentence.get(classOf[TokensAnnotation])).asScala.toList
  }

  def word(token: CoreLabel) = token.get(classOf[TextAnnotation])

  def pos(token: CoreLabel) = token.get(classOf[PartOfSpeechAnnotation])

  def namedentity(token: CoreLabel) =
    token.get(classOf[NamedEntityTagAnnotation])
}

// The part below is for testing.
object CoreNLPTest extends App {
  import provingground.CoreNLP._

  val props = new java.util.Properties()
  props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
  val pipeline = new StanfordCoreNLP(props)

  // read some text in the text variable
  val text = "Quick brown for jumps over the lazy dog"

  // create an empty Annotation just with the given text
  val document = new Annotation(text)

  // run all Annotators on this text
  pipeline.annotate(document)

  // these are all the sentences in this document
  // a CoreMap is essentially a Map that uses class objects as keys and has values with custom types
  val sentencesJava =
    (document.get(classOf[CoreAnnotations.SentencesAnnotation]))
  import scala.collection.JavaConverters._
  val sentences: List[CoreMap] = sentencesJava.asScala.toList

  val depTrees = for (sentence <- sentences) yield {
    // traversing the words in the current sentence
    // a CoreLabel is a CoreMap with additional token-specific methods
//      for (CoreLabel token: sentence.get(TokensAnnotation.class)) {
    // this is the text of the token
//        String word = token.get(TextAnnotation.class);
    // this is the POS tag of the token
//        String pos = token.get(PartOfSpeechAnnotation.class);
    // this is the NER label of the token
//       String ne = token.get(NamedEntityTagAnnotation.class);
//      }

    // this is the parse tree of the current sentence
//      Tree tree = sentence.get(TreeAnnotation.class);

    // this is the Stanford dependency graph of the current sentence
    val dependencies =
      sentence.get(classOf[CollapsedCCProcessedDependenciesAnnotation]);
    val dependencyIterable = dependencies.edgeIterable().asScala
    dependencyIterable map
      ((e: SemanticGraphEdge) => DepRel(gov(e), dep(e), depType(e)))
  }
  for (t <- depTrees; e <- t) println(e)

  // This is the coreference link graph
  // Each chain stores a set of mentions that link to each other,
  // along with a method for getting the most representative mention
  // Both sentence and token offsets start at 1!
//    Map<Integer, CorefChain> graph =
//      document.get(CorefChainAnnotation.class);

  val tree = proseTrees(
    "if a prime number p divides mn, p divides one of m and n").head
//		println(toFormula(tree, Global))
}
