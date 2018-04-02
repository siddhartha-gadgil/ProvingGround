package provingground.andrewscurtis

import provingground._, learning._

import FreeGroups._

import upickle.default.{ReadWriter => RW, macroRW}

// import Moves._

/**
  * An element in the Andrews-Curtis evolution,
  * namely:
  * * actor name,
  * * number of loops,
  * * rank
  * * associated presentation
  * * weight in distribution
  */
case class ACElem(name: String,
                  moves: Moves,
                  rank: Int,
                  pres: Presentation,
                  weight: Double,
                  loops: Int)

import ACElem._

case class Param(rank: Int = 2, size: Int = 1000, wrdCntn: Double = 0.1)

object Param{
  implicit def rw: RW[Param] = macroRW
}

object ACElem {
  type Snap =
    SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]

  object Snap{
    implicit def rw: RW[Snap] = macroRW
  }

  def toPresentation(rank: Int, fdV: FiniteDistribution[Moves]) =
    fdV map ((v: Moves) => Moves.actOnTriv(rank)(v).get)

  val fromSnap = (snap: Snap) => {
    val d = snap.state._2
    d.supp map
      ((x) => {
        val rank = snap.param.rank
        ACElem(snap.name,
               x,
               rank,
               Moves.actOnTriv(rank)(x).get,
               d(x),
               snap.loops)
      })
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

import upickle.default.{ReadWriter => RW, macroRW}

object ACThm {
  implicit def rw: RW[ACThm] = macroRW

  val fromSnap = (snap: Snap) => {
    val rank = snap.param.rank
    val d    = toPresentation(rank, snap.state._2)
    d.supp map
      ((x) => {
        ACThm(snap.name, x, d(x), snap.loops)
      })
  }

  def weight(thms: Vector[ACThm], pres: Presentation, step: Int) = {
    (thms filter ((thm) => thm.pres == pres && thm.loops == step)).headOption map
      (_.weight) getOrElse (0.0)
  }

  def weightVector(thms: Vector[ACThm], loops: Int) =
    (pres: Presentation) =>
      (1 to (loops - 1)).toVector map (weight(thms, pres, _))
}
