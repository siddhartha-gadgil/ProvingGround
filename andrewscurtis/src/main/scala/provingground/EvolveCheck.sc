package provingground

object EvolveCheck {
  println("Welcome to the Scala worksheet") //> Welcome to the Scala worksheet

  import andrewscurtis._

  import ACevolution._

  val e2 = evolve(2, 1) //> e2  : (provingground.Collections.FiniteDistribution[provingground.andrewscur
  //| tis.AtomicMove], provingground.Collections.FiniteDistribution[provingground.
  //| andrewscurtis.Moves]) = ([Conj(1,1) : 0.05263157894736842, RtMult(1,1) : 0.0
  //| 5263157894736842, LftMult(1,1) : 0.05263157894736842, Conj(0,-2) : 0.0526315
  //| 7894736842, RtMult(0,0) : 0.05263157894736842, LftMult(0,0) : 0.052631578947
  //| 36842, Conj(1,-2) : 0.05263157894736842, Conj(0,-1) : 0.05263157894736842, C
  //| onj(0,2) : 0.05263157894736842, RtMult(1,0) : 0.05263157894736842, LftMult(1
  //| ,0) : 0.05263157894736842, Conj(0,1) : 0.05263157894736842, RtMult(0,1) : 0.
  //| 05263157894736842, LftMult(0,1) : 0.05263157894736842, Inv(0) : 0.0526315789
  //| 4736842, Conj(1,-1) : 0.05263157894736842, Id() : 0.05263157894736842, Conj(
  //| 1,2) : 0.05263157894736842, Inv(1) : 0.05263157894736842],[Moves(List(Inv(0)
  //| )) : 0.05263157894736842, Moves(List(Conj(1,-1))) : 0.05263157894736842, Mov
  //| es(List(RtMult(0,0))) :
  //| Output exceeds cutoff limit.
  // the error
  e2._2 //> res0: provingground.Collections.FiniteDistribution[provingground.andrewscurt
  //| is.Moves] = [Moves(List(Inv(0))) : 0.05263157894736842, Moves(List(Conj(1,-1
  //| ))) : 0.05263157894736842, Moves(List(RtMult(0,0))) : 0.05263157894736842, M
  //| oves(List(LftMult(0,0))) : 0.05263157894736842, Moves(List(RtMult(1,0))) : 0
  //| .05263157894736842, Moves(List(LftMult(1,0))) : 0.05263157894736842, Moves(L
  //| ist(Conj(1,-2))) : 0.05263157894736842, Moves(List(Conj(1,2))) : 0.052631578
  //| 94736842, Moves(List()) : 0.05263157894736842, Moves(List(Conj(0,-1))) : 0.0
  //| 5263157894736842, Moves(List(LftMult(1,1))) : 0.05263157894736842, Moves(Lis
  //| t(RtMult(1,1))) : 0.05263157894736842, Moves(List(Conj(1,1))) : 0.0526315789
  //| 4736842, Moves(List(LftMult(0,1))) : 0.05263157894736842, Moves(List(RtMult(
  //| 0,1))) : 0.05263157894736842, Moves(List(Conj(0,1))) : 0.05263157894736842,
  //| Moves(List(Conj(0,-2))) : 0.05263157894736842, Moves(List(Inv(1))) : 0.05263
  //| 157894736842, Moves(List
  //| Output exceeds cutoff limit.

  import Moves._

  import MoveGenerator._

  import DiffStructure._

  val ms = genAllMoves(2, 2) //> ms  : List[provingground.andrewscurtis.AtomicMove] = List(Id(), Inv(0), Inv(
  //| 1), LftMult(0,0), LftMult(0,1), LftMult(1,0), LftMult(1,1), RtMult(0,0), RtM
  //| ult(0,1), RtMult(1,0), RtMult(1,1), Conj(0,1), Conj(0,-1), Conj(0,2), Conj(0
  //| ,-2), Conj(1,1), Conj(1,-1), Conj(1,2), Conj(1,-2))
  unifMoves(2) //> res1: provingground.Collections.FiniteDistribution[provingground.andrewscurt
  //| is.AtomicMove] = [Conj(1,1) : 0.05263157894736842, RtMult(1,1) : 0.052631578
  //| 94736842, LftMult(1,1) : 0.05263157894736842, Conj(0,-2) : 0.052631578947368
  //| 42, RtMult(0,0) : 0.05263157894736842, LftMult(0,0) : 0.05263157894736842, C
  //| onj(1,-2) : 0.05263157894736842, Conj(0,-1) : 0.05263157894736842, Conj(0,2)
  //|  : 0.05263157894736842, RtMult(1,0) : 0.05263157894736842, LftMult(1,0) : 0.
  //| 05263157894736842, Conj(0,1) : 0.05263157894736842, RtMult(0,1) : 0.05263157
  //| 894736842, LftMult(0,1) : 0.05263157894736842, Inv(0) : 0.05263157894736842,
  //|  Conj(1,-1) : 0.05263157894736842, Id() : 0.05263157894736842, Conj(1,2) : 0
  //| .05263157894736842, Inv(1) : 0.05263157894736842]

  trivMoveSeq //> res2: provingground.Collections.FiniteDistribution[provingground.andrewscurt
  //| is.Moves] = [Moves(List()) : 1.0]
  val m = genMoveFn(ms(2)) //> m  : provingground.DiffbleFunction[provingground.Collections.FiniteDistribut
  //| ion[provingground.andrewscurtis.Moves],provingground.Collections.FiniteDistr
  //| ibution[provingground.andrewscurtis.Moves]] = provingground.DiffbleFunction$
  //| $anon$1@2bd7f521

  m(trivMoveSeq) //> res3: provingground.Collections.FiniteDistribution[provingground.andrewscurt
  //| is.Moves] = [Moves(List(Inv(1))) : 1.0]

  val mm = genWtDyn(ms(2)) //> mm  : provingground.DiffbleFunction[(provingground.Collections.FiniteDistrib
  //| ution[provingground.andrewscurtis.AtomicMove], provingground.Collections.Fin
  //| iteDistribution[provingground.andrewscurtis.Moves]),provingground.Collection
  //| s.FiniteDistribution[provingground.andrewscurtis.Moves]] = provingground.Dif
  //| fbleFunction$$anon$1@6f1e6003

  mm((unifMoves(2), trivMoveSeq)) //> res4: provingground.Collections.FiniteDistribution[provingground.andrewscurt
  //| is.Moves] = [Moves(List(Inv(1))) : 0.05263157894736842]

  val mmm = genWtDyn(ms(2)) //> mmm  : provingground.DiffbleFunction[(provingground.Collections.FiniteDistri
  //| bution[provingground.andrewscurtis.AtomicMove], provingground.Collections.Fi
  //| niteDistribution[provingground.andrewscurtis.Moves]),provingground.Collectio
  //| ns.FiniteDistribution[provingground.andrewscurtis.Moves]] = provingground.Di
  //| ffbleFunction$$anon$1@33319df3

  mmm == mm //> res5: Boolean = false

  mmm((unifMoves(2), trivMoveSeq)) //> res6: provingground.Collections.FiniteDistribution[provingground.andrewscurt
  //| is.Moves] = [Moves(List(Inv(1))) : 0.05263157894736842]

  unifMoves(2)(ms(2)) //> res7: Double = 0.05263157894736842

  ms(2) //> res8: provingground.andrewscurtis.AtomicMove = Inv(1)

}
