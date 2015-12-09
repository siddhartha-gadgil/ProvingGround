package provingground.andrewscurtis

import provingground._

import FreeGroups._

import Moves._


/**
 * An element in the Andrews-Curtis evolution,
 * namely:
 * * actor name,
 * * number of loops,
 * * rank
 * * associated presentation
 * * weight in distribution
 */
case class ACElem(name: String, moves: Moves, rank: Int, pres: Presentation, weight: Double, loops: Int)


import ACElem._

case class Param(
       rank: Int = 2, size : Int = 1000, wrdCntn: Double = 0.1,
       dir : String = "acDev"
       )

object ACElem{
  type Snap = SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]

  def toPresentation(rank: Int, fdV: FiniteDistribution[Moves]) = fdV map ((v: Moves) => Moves.actOnTriv(rank)(v).get)


  val fromSnap =
    (snap: Snap) =>
  {
    val d = snap.state._2
    d.supp map ((x) => {
      val rank = snap.param.rank
      ACElem(snap.name, x, rank, Moves.actOnTriv(rank)(x).get, d(x), snap.loops)
      } )
  }
}

/**
 * An presentation (theorem) in the Andrews-Curtis evolution,
 * namely:
 * * actor name,
 * * number of loops,
 * * presentation
 * * weight in distribution on presentations
 */
case class ACThm(name: String, pres: Presentation, weight: Double, loops: Int)

object ACThm{
  val fromSnap =
    (snap: Snap) =>
      { val rank = snap.param.rank
        val d = toPresentation(rank, snap.state._2)
        d.supp map ((x) => {
        ACThm(snap.name, x, d(x), snap.loops)
      } )
  }
      
  def weight(thms : Vector[ACThm], pres: Presentation, step: Int) = {
    (thms filter ((thm) => thm.pres == pres && thm.loops == step)).headOption map (_.weight) getOrElse(0.0)
  }
  
  def weightVector(thms: Vector[ACThm], loops: Int) = (pres: Presentation) => 
    (0 until loops).toVector map (weight(thms, pres, _))
}
