package worksheets
import provingground.CoreNLP._
import provingground.CoreNLPTest

object NLPtesting {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
   
  val pipe = newPipe                              //> Adding annotator tokenize
                                                  //| Adding annotator ssplit
                                                  //| Adding annotator pos
                                                  //| Reading POS tagger model from edu/stanford/nlp/models/pos-tagger/english-lef
                                                  //| t3words/english-left3words-distsim.tagger ... done [3.0 sec].
                                                  //| Adding annotator lemma
                                                  //| Adding annotator ner
                                                  //| Loading classifier from edu/stanford/nlp/models/ner/english.all.3class.dists
                                                  //| im.crf.ser.gz ... done [7.4 sec].
                                                  //| Loading classifier from edu/stanford/nlp/models/ner/english.muc.7class.dists
                                                  //| im.crf.ser.gz ... done [4.1 sec].
                                                  //| Loading classifier from edu/stanford/nlp/models/ner/english.conll.4class.dis
                                                  //| tsim.crf.ser.gz ... done [6.5 sec].
                                                  //| Reading TokensRegex rules from edu/stanford/nlp/models/sutime/defs.sutime.tx
                                                  //| t
                                                  //| Reading TokensRegex rules from edu/stanford/nlp/models/sutime/english.sutime
                                                  //| .txt
                                                  //| Jul 31, 2014 7:51:22 PM edu.stanford.nlp.ling.tokensregex.CoreMapExpressionE
                                                  //| xtractor appendRules
                                                  //| INFO: Ignoring inactive rule: null
                                                  //| Jul 31, 2014 7:51:22
                                                  //| Output exceeds cutoff limit.
                                                  
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
                                                  //| , prep_of((order,42),(G,
                                                  //| Output exceeds cutoff limit.
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
                                                  //| ), det((G,47),(a,44)), a
                                                  //| Output exceeds cutoff limit.
  trees.head.tree.length                          //> res0: Int = 41
}