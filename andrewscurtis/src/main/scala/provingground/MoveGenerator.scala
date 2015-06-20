package provingground.andrewscurtis

import provingground.andrewscurtis.FreeGroups._

object MoveGenerator {
  
  def genAllInv(sz: Int): List[Presentation => Presentation] = {
    (0 until sz).toList map ((x:Int) => ((pres: Presentation) => Presentation.inv(pres, x)))
  }

  def genLftMult(sz: Int): List[Presentation => Presentation] = {
    val f = ((k: Int) => ((0 until sz).toList map ((l: Int) => ((pres: Presentation) => Presentation.lftmult(pres, k, l)))))
    (0 until sz).toList flatMap f
  }

  def genRtMult(sz: Int): List[Presentation => Presentation] = {
    val f = ((k: Int) => ((0 until sz).toList map ((l: Int) => ((pres: Presentation) => Presentation.rtmult(pres, k, l)))))
    (0 until sz).toList flatMap f
  }

  def genConjRelators(sz: Int): List[Presentation => Presentation] = {
    val f = ((k: Int) => ((0 until sz).toList map ((l: Int) => ((pres: Presentation) => Presentation.conjRelators(pres, k, l)))))
    (0 until sz).toList flatMap f
  }

  def genAllMoves(rank: Int, sz: Int): List[Presentation => Presentation] = {
    val takenone = List(Presentation.id(_))
    val takesz = List(genAllInv(_), genLftMult(_), genRtMult(_), genConjRelators(_))
    val sz_functions = takesz flatMap ((f: Int => List[Presentation => Presentation]) => f(sz))
    takenone ++ sz_functions
  }
}
