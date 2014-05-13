package main.scala
import provingGround.CoreNLP._

object NLPtesting {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val pipe = newPipe                              //> Adding annotator tokenize
                                                  //| Adding annotator ssplit
                                                  //| Adding annotator pos
                                                  //| Reading POS tagger model from edu/stanford/nlp/models/pos-tagger/english-lef
                                                  //| t3words/english-left3words-distsim.tagger ... done [0.8 sec].
                                                  //| Adding annotator lemma
                                                  //| Adding annotator ner
                                                  //| Loading classifier from edu/stanford/nlp/models/ner/english.all.3class.dists
                                                  //| im.crf.ser.gz ... done [3.1 sec].
                                                  //| Loading classifier from edu/stanford/nlp/models/ner/english.muc.7class.dists
                                                  //| im.crf.ser.gz ... done [2.5 sec].
                                                  //| Loading classifier from edu/stanford/nlp/models/ner/english.conll.4class.dis
                                                  //| tsim.crf.ser.gz ... done [1.7 sec].
                                                  //| Reading TokensRegex rules from edu/stanford/nlp/models/sutime/defs.sutime.tx
                                                  //| t
                                                  //| Reading TokensRegex rules from edu/stanford/nlp/models/sutime/english.sutime
                                                  //| .txt
                                                  //| May 13, 2014 10:54:44 AM edu.stanford.nlp.ling.tokensregex.CoreMapExpression
                                                  //| Extractor appendRules
                                                  //| INFO: Ignoring inactive rule: null
                                                  //| May 13, 2014 10:54:44 AM edu.sta
                                                  //| Output exceeds cutoff limit.
                                                  
  val text ="This is a simple sentence"           //> text  : String = This is a simple sentence
  
  val trees = proseTrees(text, pipe)              //> trees  : List[provingGround.NlpProse.ProseTree] = List((sentence,5):List(nsu
                                                  //| bj((sentence,5),(This,1)), cop((sentence,5),(is,2)), det((sentence,5),(a,3))
                                                  //| , amod((sentence,5),(simple,4))))
  println (trees)                                 //> List((sentence,5):List(nsubj((sentence,5),(This,1)), cop((sentence,5),(is,2)
                                                  //| ), det((sentence,5),(a,3)), amod((sentence,5),(simple,4))))
  trees.head.tree.length                          //> res0: Int = 4
}