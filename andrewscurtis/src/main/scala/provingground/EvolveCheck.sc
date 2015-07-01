package provingground

object EvolveCheck {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import andrewscurtis._
  
  import ACevolution._
  
  val e2 = evolve(2, 1)                           //> e2  : (provingground.Collections.FiniteDistribution[provingground.andrewscur
                                                  //| tis.Moves => Option[provingground.andrewscurtis.Moves]], provingground.Colle
                                                  //| ctions.FiniteDistribution[provingground.andrewscurtis.Moves]) = ([<function1
                                                  //| > : 0.05263157894736842, <function1> : 0.05263157894736842, <function1> : 0.
                                                  //| 05263157894736842, <function1> : 0.05263157894736842, <function1> : 0.052631
                                                  //| 57894736842, <function1> : 0.05263157894736842, <function1> : 0.052631578947
                                                  //| 36842, <function1> : 0.05263157894736842, <function1> : 0.05263157894736842,
                                                  //|  <function1> : 0.05263157894736842, <function1> : 0.05263157894736842, <func
                                                  //| tion1> : 0.05263157894736842, <function1> : 0.05263157894736842, <function1>
                                                  //|  : 0.05263157894736842, <function1> : 0.05263157894736842, <function1> : 0.0
                                                  //| 5263157894736842, <function1> : 0.05263157894736842, <function1> : 0.0526315
                                                  //| 7894736842, <function1> : 0.05263157894736842],[])
  // the error
	e2._2                                     //> res0: provingground.Collections.FiniteDistribution[provingground.andrewscurt
                                                  //| is.Moves] = []

	import Moves._
	
	import MoveGenerator._
	
	import DiffStructure._
	
	val ms = genAllMoves(2, 2)                //> ms  : List[provingground.andrewscurtis.AtomicMove] = List(Id(), Inv(0), Inv(
                                                  //| 1), LftMult(0,0), LftMult(0,1), LftMult(1,0), LftMult(1,1), RtMult(0,0), RtM
                                                  //| ult(0,1), RtMult(1,0), RtMult(1,1), Conj(0,1), Conj(0,-1), Conj(0,2), Conj(0
                                                  //| ,-2), Conj(1,1), Conj(1,-1), Conj(1,2), Conj(1,-2))
	unifMoves(2)                              //> res1: provingground.Collections.FiniteDistribution[provingground.andrewscurt
                                                  //| is.Moves => Option[provingground.andrewscurtis.Moves]] = [<function1> : 0.05
                                                  //| 263157894736842, <function1> : 0.05263157894736842, <function1> : 0.05263157
                                                  //| 894736842, <function1> : 0.05263157894736842, <function1> : 0.05263157894736
                                                  //| 842, <function1> : 0.05263157894736842, <function1> : 0.05263157894736842, <
                                                  //| function1> : 0.05263157894736842, <function1> : 0.05263157894736842, <functi
                                                  //| on1> : 0.05263157894736842, <function1> : 0.05263157894736842, <function1> :
                                                  //|  0.05263157894736842, <function1> : 0.05263157894736842, <function1> : 0.052
                                                  //| 63157894736842, <function1> : 0.05263157894736842, <function1> : 0.052631578
                                                  //| 94736842, <function1> : 0.05263157894736842, <function1> : 0.052631578947368
                                                  //| 42, <function1> : 0.05263157894736842]
	
	trivMoveSeq                               //> res2: provingground.Collections.FiniteDistribution[provingground.andrewscurt
                                                  //| is.Moves] = [Moves(List()) : 1.0]
	val m = genMoveFn(ms(2))                  //> m  : provingground.DiffbleFunction[provingground.Collections.FiniteDistribut
                                                  //| ion[provingground.andrewscurtis.Moves],provingground.Collections.FiniteDistr
                                                  //| ibution[provingground.andrewscurtis.Moves]] = provingground.DiffbleFunction$
                                                  //| $anon$1@4e345c87
	
	m(trivMoveSeq)                            //> res3: provingground.Collections.FiniteDistribution[provingground.andrewscurt
                                                  //| is.Moves] = [Moves(List(Inv(1))) : 1.0]
	
	val mm = genWtDyn(ms(2))                  //> mm  : provingground.DiffbleFunction[(provingground.Collections.FiniteDistrib
                                                  //| ution[provingground.andrewscurtis.Moves => Option[provingground.andrewscurti
                                                  //| s.Moves]], provingground.Collections.FiniteDistribution[provingground.andrew
                                                  //| scurtis.Moves]),provingground.Collections.FiniteDistribution[provingground.a
                                                  //| ndrewscurtis.Moves]] = provingground.DiffbleFunction$$anon$1@4fa6cb2d
	
	mm((unifMoves(2), trivMoveSeq))           //> res4: provingground.Collections.FiniteDistribution[provingground.andrewscurt
                                                  //| is.Moves] = [Moves(List(Inv(1))) : 0.0]

	val mmm = genWtDyn(ms(2))                 //> mmm  : provingground.DiffbleFunction[(provingground.Collections.FiniteDistri
                                                  //| bution[provingground.andrewscurtis.Moves => Option[provingground.andrewscurt
                                                  //| is.Moves]], provingground.Collections.FiniteDistribution[provingground.andre
                                                  //| wscurtis.Moves]),provingground.Collections.FiniteDistribution[provingground.
                                                  //| andrewscurtis.Moves]] = provingground.DiffbleFunction$$anon$1@4f652035
	
	mmm == mm                                 //> res5: Boolean = false
	
	mmm((unifMoves(2), trivMoveSeq))          //> res6: provingground.Collections.FiniteDistribution[provingground.andrewscurt
                                                  //| is.Moves] = [Moves(List(Inv(1))) : 0.0]
                                                  
  unifMoves(2)(ms(2))                             //> res7: Double = 0.0
  
  ms(2)                                           //> res8: provingground.andrewscurtis.AtomicMove = Inv(1)
  
}