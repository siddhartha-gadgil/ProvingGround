package provingground.andrewscurtis

import provingground.FreeGroups._
import provingground.andrewscurtis.Moves._

object MoveGenerator {

  def genAllInv(sz: Int): List[AtomicMove] = {
    (0 until sz).toList map ((x:Int) => Inv(x))
  }

  def genLftMult(sz: Int): List[AtomicMove] = {
    (for (j <- 0 until sz; k <- 0 until sz if j != k) yield LftMult(j, k)).toList
  }

  def genRtMult(sz: Int): List[AtomicMove] = {
    (for (j <- 0 until sz; k <- 0 until sz if j != k) yield RtMult(j, k)).toList
  }

  def genConj(rank: Int, sz: Int): List[AtomicMove] = {
    val all_gens = (1 to rank).toList flatMap ((x: Int) => List(x, -1*x))
    val f = ((k: Int) => (all_gens map ((l: Int) => Conj(k,l))))
    (0 until sz).toList flatMap f
  }

  def genAllMoves(rank: Int, sz: Int): List[AtomicMove] = {
    val id = List(Id)
    val inv = genAllInv(sz)
    val lftmult = genLftMult(sz)
    val rtmult = genRtMult(sz)
    val conj = genConj(rank, sz)
    id ++ inv ++ lftmult ++ rtmult ++ conj
  }
}
