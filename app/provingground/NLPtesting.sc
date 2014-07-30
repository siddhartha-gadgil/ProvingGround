package src.main.scala
import provingground.CoreNLP._
import provingground.CoreNLPTest

object NLPtesting {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
   
  val pipe = newPipe                              //> Adding annotator tokenize
                                                  //| Adding annotator ssplit
                                                  //| Adding annotator pos
                                                  //| Reading POS tagger model from edu/stanford/nlp/models/pos-tagger/english-lef
                                                  //| t3words/english-left3words-distsim.tagger ... done [1.2 sec].
                                                  //| Adding annotator lemma
                                                  //| Adding annotator ner
                                                  //| Loading classifier from edu/stanford/nlp/models/ner/english.all.3class.dists
                                                  //| im.crf.ser.gz ... done [3.3 sec].
                                                  //| Loading classifier from edu/stanford/nlp/models/ner/english.muc.7class.dists
                                                  //| im.crf.ser.gz ... done [2.0 sec].
                                                  //| Loading classifier from edu/stanford/nlp/models/ner/english.conll.4class.dis
                                                  //| tsim.crf.ser.gz ... done [1.7 sec].
                                                  //| Reading TokensRegex rules from edu/stanford/nlp/models/sutime/defs.sutime.tx
                                                  //| t
                                                  //| Reading TokensRegex rules from edu/stanford/nlp/models/sutime/english.sutime
                                                  //| .txt
                                                  //| Jul 28, 2014 11:19:34 AM edu.stanford.nlp.ling.tokensregex.CoreMapExpression
                                                  //| Extractor appendRules
                                                  //| INFO: Ignoring inactive rule: null
                                                  //| Jul 28, 2014 11:19:34 AM edu.stanford.nlp.ling.tokensregex.CoreMapExpression
                                                  //| Extractor appendRules
                                                  //| INFO: Ignoring inactive rule: temporal-composite-8:ranges
                                                  //| Reading TokensRegex rules from edu/stanford/nlp/models/sutime/english.holida
                                                  //| ys.sutime.txt
                                                  //| Initializing JollyDayHoliday for sutime with classpath:edu/stanford/nlp/mode
                                                  //| ls/sutime/jollyday/Holidays_sutime.xml
                                                  //| Reading TokensRegex rules from edu/stanford/nlp/models/sutime/defs.sutime.tx
                                                  //| t
                                                  //| Reading TokensRegex rules from edu/stanford/nlp/models/sutime/english.sutime
                                                  //| .txt
                                                  //| Jul 28, 2014 11:19:35 AM edu.stanford.nlp.ling.tokensregex.CoreMapExpression
                                                  //| Extractor appendRules
                                                  //| INFO: Ignoring inactive rule: null
                                                  //| Jul 28, 2014 11:19:35 AM edu.stanford.nlp.ling.tokensregex.CoreMapExpression
                                                  //| Extractor appendRules
                                                  //| INFO: Ignoring inactive rule: temporal-composite-8:ranges
                                                  //| Reading TokensRegex rules from edu/stanford/nlp/models/sutime/english.holida
                                                  //| ys.sutime.txt
                                                  //| Adding annotator parse
                                                  //| Loading parser from serialized file edu/stanford/nlp/models/lexparser/englis
                                                  //| hPCFG.ser.gz ... done [0.7 sec].
                                                  //| Adding annotator dcoref
                                                  //| pipe  : edu.stanford.nlp.pipeline.StanfordCoreNLP = edu.stanford.nlp.pipelin
                                                  //| e.StanfordCoreNLP@557712ec
                                                  
  val text ="While Lagrange's theorem states that for any finite group G the order (number of elements) of every subgroup of G divides the order of G, the Sylow theorems state that for any prime factor p of the order of a finite group G, there exists a Sylow p-subgroup of G"
                                                  //> text  : String = While Lagrange's theorem states that for any finite group G
                                                  //|  the order (number of elements) of every subgroup of G divides the order of 
                                                  //| G, the Sylow theorems state that for any prime factor p of the order of a fi
                                                  //| nite group G, there exists a Sylow p-subgroup of G
  
  val doc = annotatedDoc(text, pipe)              //> doc  : edu.stanford.nlp.pipeline.Annotation = While Lagrange's theorem state
                                                  //| s that for any finite group G the order (number of elements) of every subgro
                                                  //| up of G divides the order of G, the Sylow theorems state that for any prime 
                                                  //| factor p of the order of a finite group G, there exists a Sylow p-subgroup o
                                                  //| f G
  val sentence = sentences(doc).head              //> sentence  : edu.stanford.nlp.util.CoreMap = While Lagrange's theorem states 
                                                  //| that for any finite group G the order (number of elements) of every subgroup
                                                  //|  of G divides the order of G, the Sylow theorems state that for any prime fa
                                                  //| ctor p of the order of a finite group G, there exists a Sylow p-subgroup of 
                                                  //| G
  
  val labels = coreLabelList(sentence)            //> labels  : List[edu.stanford.nlp.ling.CoreLabel] = List(While-1, Lagrange-2, 
                                                  //| 's-3, theorem-4, states-5, that-6, for-7, any-8, finite-9, group-10, G-11, t
                                                  //| he-12, order-13, -LRB--14, number-15, of-16, elements-17, -RRB--18, of-19, e
                                                  //| very-20, subgroup-21, of-22, G-23, divides-24, the-25, order-26, of-27, G-28
                                                  //| , ,-29, the-30, Sylow-31, theorems-32, state-33, that-34, for-35, any-36, pr
                                                  //| ime-37, factor-38, p-39, of-40, the-41, order-42, of-43, a-44, finite-45, gr
                                                  //| oup-46, G-47, ,-48, there-49, exists-50, a-51, Sylow-52, p-subgroup-53, of-5
                                                  //| 4, G-55)
  val named = labels map (namedentity)            //> named  : List[String] = List(O, LOCATION, O, O, O, O, O, O, O, O, O, O, O, O
                                                  //| , O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O,
                                                  //|  O, O, O, O, O, O, O, O, O, O, O, O, MISC, O, O, O)
  
  val trees = proseTrees(text, pipe)              //> trees  : List[provingground.NlpProse.ProseTree] = List((state,33):List(poss(
                                                  //| (states,5),(Lagrange,2)), prep_of((states,5),(subgroup,21)), nn((states,5),(
                                                  //| theorem,4)), rcmod((states,5),(for,7)), dep((states,5),(order,13)), dep((sta
                                                  //| tes,5),(number,15)), dep((for,7),(that,6)), pobj((for,7),(G,11)), det((G,11)
                                                  //| ,(any,8)), amod((G,11),(finite,9)), nn((G,11),(group,10)), det((order,13),(t
                                                  //| he,12)), prep_of((number,15),(elements,17)), det((subgroup,21),(every,20)), 
                                                  //| prep_of((subgroup,21),(G,23)), mark((divides,24),(While,1)), nsubj((divides,
                                                  //| 24),(states,5)), dobj((divides,24),(order,26)), det((order,26),(the,25)), pr
                                                  //| ep_of((order,26),(G,28)), nn((theorems,32),(Sylow,31)), det((theorems,32),(t
                                                  //| he,30)), ccomp((state,33),(exists,50)), nsubj((state,33),(theorems,32)), adv
                                                  //| cl((state,33),(divides,24)), nn((p,39),(factor,38)), det((p,39),(any,36)), a
                                                  //| mod((p,39),(prime,37)), prep_of((p,39),(order,42)), det((order,42),(the,41))
                                                  //| , prep_of((order,42),(G,47)), nn((G,47),(group,46)), det((G,47),(a,44)), amo
                                                  //| d((G,47),(finite,45)), mark((exists,50),(that,34)), expl((exists,50),(there,
                                                  //| 49)), prep_for((exists,50),(p,39)), dobj((exists,50),(p-subgroup,53)), det((
                                                  //| p-subgroup,53),(a,51)), prep_of((p-subgroup,53),(G,55)), nn((p-subgroup,53),
                                                  //| (Sylow,52))))
  println (trees)                                 //> List((state,33):List(poss((states,5),(Lagrange,2)), prep_of((states,5),(subg
                                                  //| roup,21)), nn((states,5),(theorem,4)), rcmod((states,5),(for,7)), dep((state
                                                  //| s,5),(order,13)), dep((states,5),(number,15)), dep((for,7),(that,6)), pobj((
                                                  //| for,7),(G,11)), det((G,11),(any,8)), amod((G,11),(finite,9)), nn((G,11),(gro
                                                  //| up,10)), det((order,13),(the,12)), prep_of((number,15),(elements,17)), det((
                                                  //| subgroup,21),(every,20)), prep_of((subgroup,21),(G,23)), mark((divides,24),(
                                                  //| While,1)), nsubj((divides,24),(states,5)), dobj((divides,24),(order,26)), de
                                                  //| t((order,26),(the,25)), prep_of((order,26),(G,28)), nn((theorems,32),(Sylow,
                                                  //| 31)), det((theorems,32),(the,30)), ccomp((state,33),(exists,50)), nsubj((sta
                                                  //| te,33),(theorems,32)), advcl((state,33),(divides,24)), nn((p,39),(factor,38)
                                                  //| ), det((p,39),(any,36)), amod((p,39),(prime,37)), prep_of((p,39),(order,42))
                                                  //| , det((order,42),(the,41)), prep_of((order,42),(G,47)), nn((G,47),(group,46)
                                                  //| ), det((G,47),(a,44)), amod((G,47),(finite,45)), mark((exists,50),(that,34))
                                                  //| , expl((exists,50),(there,49)), prep_for((exists,50),(p,39)), dobj((exists,5
                                                  //| 0),(p-subgroup,53)), det((p-subgroup,53),(a,51)), prep_of((p-subgroup,53),(G
                                                  //| ,55)), nn((p-subgroup,53),(Sylow,52))))
  trees.head.tree.length                          //> res0: Int = 41
}