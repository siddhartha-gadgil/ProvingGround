package provingground.andrewscurtis

import provingground.andrewscurtis.FreeGroups._
import provingground.andrewscurtis.Moves._

object MoveGenerator {
  
  def genAllInv(sz: Int): List[MoveFunction] = {
    (0 until sz).toList map ((x:Int) => Inv(x))
  }

  def genLftMult(sz: Int): List[MoveFunction] = {
    val f = ((k: Int) => ((0 until sz).toList map ((l: Int) => LftMult(k,l))))
    (0 until sz).toList flatMap f
  }

  def genRtMult(sz: Int): List[MoveFunction] = {
    val f = ((k: Int) => ((0 until sz).toList map ((l: Int) => RtMult(k,l))))
    (0 until sz).toList flatMap f
  }

  def genConj(rank: Int, sz: Int): List[MoveFunction] = {
    val f = ((k: Int) => ((1 to rank).toList map ((l: Int) => Conj(k,l))))
    (0 until sz).toList flatMap f
  }

  def genAllMoves(rank: Int, sz: Int): List[MoveFunction] = {
    val id = List(Id())
    val inv = genAllInv(sz)
    val lftmult = genLftMult(sz)
    val rtmult = genRtMult(sz)
    val conj = genConj(rank, sz)
    id ++ inv ++ lftmult ++ rtmult ++ conj
  }
}
