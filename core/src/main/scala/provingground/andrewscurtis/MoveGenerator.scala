package provingground.andrewscurtis

import provingground.andrewscurtis.FreeGroups._
import provingground.andrewscurtis.Moves._

import provingground.learning.Collections._

import provingground._, learning._

object MoveGenerator {

  def genAllInv(sz: Int): List[AtomicMove] = {
    (0 until sz).toList map ((x: Int) => Inv(x))
  }

  def genLftMult(sz: Int): List[AtomicMove] = {
    (for (j <- 0 until sz; k <- 0 until sz if j != k)
      yield LftMult(j, k)).toList
  }

  def genRtMult(sz: Int): List[AtomicMove] = {
    (for (j <- 0 until sz; k <- 0 until sz if j != k)
      yield RtMult(j, k)).toList
  }

  def genConj(rank: Int, sz: Int): List[AtomicMove] = {
    val all_gens = (1 to rank).toList flatMap ((x: Int) => List(x, -1 * x))
    val f        = ((k: Int) => (all_gens map ((l: Int) => Conj(k, l))))
    (0 until sz).toList flatMap f
  }

  def genLftInvMult(rank: Int): List[AtomicMove] = {
    (for (j <- 0 until rank; k <- 0 until rank if j != k)
      yield LftMultInv(j, k)).toList
  }

  def genRtInvMult(rank: Int): List[AtomicMove] = {
    (for (j <- 0 until rank; k <- 0 until rank if j != k)
      yield RtMultInv(j, k)).toList
  }

  def genTranspose(rank: Int): List[AtomicMove] = {
    (for (j <- 0 until rank; k <- 0 until rank if j != k)
      yield Transpose(j, k)).toList
  }

  def genAllMoves(rank: Int, sz: Int): List[AtomicMove] = {
    val id      = List(Id)
    val inv     = genAllInv(sz)
    val lftmult = genLftMult(sz)
    val rtmult  = genRtMult(sz)
    val conj    = genConj(rank, sz)
    id ++ inv ++ lftmult ++ rtmult ++ conj
  }

  val E = Weighted(Moves.empty, 1)

//  lazy val eSet = FiniteDistributionSet(Set(E))

  lazy val eVec = FiniteDistribution(Vector(E))

//  lazy val eParVec = FiniteDistributionParVec(Vector(E).par)

  def allMoves(rank: Int) = genAllMoves(rank, rank)

  def unifMoves(rank: Int) = FiniteDistribution.uniform(allMoves(rank))

  def extendedMovesList(rank: Int): List[AtomicMove] = {
    val id         = List(Id)
    val inv        = genAllInv(rank)
    val lftmult    = genLftMult(rank)
    val rtmult     = genRtMult(rank)
    val conj       = genConj(rank, rank)
    val lftmultinv = genLftInvMult(rank)
    val rtmultinv  = genRtInvMult(rank)
    val transp     = genTranspose(rank)
    id ++ inv ++ lftmult ++ rtmult ++ conj ++ lftmultinv ++ rtmultinv ++ transp
  }

  def learnerMoves(rank: Int) = {
    val inv        = genAllInv(rank)
    val lftmult    = genLftMult(rank)
    val rtmult     = genRtMult(rank)
    val conj       = genConj(rank, rank)
    val lftmultinv = genLftInvMult(rank)
    val rtmultinv  = genRtInvMult(rank)
    val transp     = genTranspose(rank)
    val moves =
      inv ++ lftmult ++ rtmult ++ conj ++ lftmultinv ++ rtmultinv ++ transp
    val moveWeight = 0.5 / (moves.size)
    val pmf =
      Weighted(Id: AtomicMove, 0.5) :: (moves map (Weighted(_, moveWeight)))
    FiniteDistribution(pmf)
  }

  def extendedMoves(rank: Int) =
    FiniteDistribution.uniform(extendedMovesList(rank))
}
