package src.main.scala

import provingground.AndrewsCurtis._
import provingground.AndrewsCurtis
import provingground.FreeGroups._
import provingground.Collections._

object AndrewsCurtisExplore {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  Presentation(List(), 0)                         //> res0: provingground.FreeGroups.Presentation = <; >
  
  nullpres                                        //> res1: provingground.FreeGroups.Presentation = <; >
  
  /*
  val baseDstbn : DynDstbn = {
    val vrtdst = FiniteDistribution(Seq(Weighted(nullpres, 1.0)))
    val edgseq = for (mvtyp <- MoveTypeList) yield Weighted(mvtyp, 1.0/ MoveTypeList.length)
    val edgdst  = FiniteDistribution(edgseq)
    DynDst(vrtdst, edgdst, 0.3)
  } */
  baseDstbn                                       //> res2: provingground.AndrewsCurtis.DynDst[provingground.FreeGroups.Presentati
                                                  //| on,provingground.AndrewsCurtis.ACMoveType] = DynDst([<; > : 1.0],[ACStabMv :
                                                  //|  0.16666666666666666, ACDeStabMv : 0.16666666666666666, RtMultMv : 0.1666666
                                                  //| 6666666666, LftMultMv : 0.16666666666666666, ConjMv : 0.16666666666666666, I
                                                  //| nvMv : 0.16666666666666666],0.3)
  
}