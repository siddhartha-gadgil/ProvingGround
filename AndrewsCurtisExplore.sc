package src.main.scala

import provingGround.AndrewsCurtis._
import provingGround.AndrewsCurtis
import provingGround.FreeGroups._
import provingGround.Collections._

object AndrewsCurtisExplore {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  Presentation(List(), 0)                         //> res0: provingGround.FreeGroups.Presentation = <; >
  
  nullpres                                        //> res1: provingGround.FreeGroups.Presentation = <; >
  
  /*
  val baseDstbn : DynDstbn = {
    val vrtdst = FiniteDistribution(Seq(Weighted(nullpres, 1.0)))
    val edgseq = for (mvtyp <- MoveTypeList) yield Weighted(mvtyp, 1.0/ MoveTypeList.length)
    val edgdst  = FiniteDistribution(edgseq)
    DynDst(vrtdst, edgdst, 0.3)
  } */
  baseDstbn                                       //> res2: provingGround.AndrewsCurtis.DynDst[provingGround.FreeGroups.Presentati
                                                  //| on,provingGround.AndrewsCurtis.ACMoveType] = DynDst([<; > : 1.0],[ACStabMv :
                                                  //|  0.16666666666666666, ACDeStabMv : 0.16666666666666666, RtMultMv : 0.1666666
                                                  //| 6666666666, LftMultMv : 0.16666666666666666, ConjMv : 0.16666666666666666, I
                                                  //| nvMv : 0.16666666666666666],0.3)
  
}