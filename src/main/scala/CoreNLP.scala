package provingGround

import java.io._
// import java.util.*;
import scala.collection.JavaConverters._

import edu.stanford.nlp.io._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.trees._
import edu.stanford.nlp.util._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.trees.semgraph._
import edu.stanford.nlp.trees.semgraph.SemanticGraphCoreAnnotations._

object CoreNLP extends App{
		def gov(e: SemanticGraphEdge) = (e.getGovernor().word, e.getGovernor().index)
		def dep(e: SemanticGraphEdge) = (e.getDependent().word, e.getDependent().index)
		def depType(e: SemanticGraphEdge) = (e.getRelation().getShortName(), e.getRelation.getSpecific())

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
    val sentencesJava = (document.get[java.util.List[CoreMap],CoreAnnotations.SentencesAnnotation](classOf[CoreAnnotations.SentencesAnnotation]))
    import scala.collection.JavaConverters._
    val sentences: List[CoreMap] = sentencesJava.asScala.toList
    
    val depTrees = for(sentence <-sentences) yield { 
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
      val dependencies = sentence.get[SemanticGraph, CollapsedCCProcessedDependenciesAnnotation](classOf[CollapsedCCProcessedDependenciesAnnotation]);
			val dependencyIterable = dependencies.edgeIterable().asScala
			dependencyIterable map ((e: SemanticGraphEdge) => (depType(e), gov(e), dep(e)))  
    }
		for (t<-depTrees; e<- t) println(e)

    // This is the coreference link graph
    // Each chain stores a set of mentions that link to each other,
    // along with a method for getting the most representative mention
    // Both sentence and token offsets start at 1!
//    Map<Integer, CorefChain> graph = 
//      document.get(CorefChainAnnotation.class);
	
}
