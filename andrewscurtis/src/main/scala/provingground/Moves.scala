package provingground.andrewscurtis

import provingground.andrewscurtis.FreeGroups._
import scala.language.implicitConversions
import Moves._

sealed trait MoveFunction {
  def apply(pres: Presentation): Presentation
  def apply(moves: Moves): Moves = toMoves(this) compose moves
  def toFunc: Presentation => Presentation = ((x: Presentation) => this(x))
  def compose(mf: MoveFunction): Moves = {
    toMoves(this) compose toMoves(mf)
  }
  def compose(func: Presentation => Presentation): Presentation => Presentation = {
    ((x: Presentation) => this(func(x)))
  }
}

case class Id() extends MoveFunction {
  override def apply(pres: Presentation) = pres
}

case class Inv(k: Int) extends MoveFunction {
  override def apply(pres: Presentation) = pres.inv(k)
}

case class RtMult(k: Int, l: Int) extends MoveFunction {
  override def apply(pres: Presentation) = pres.rtmult(k,l)
}

case class LftMult(k: Int, l: Int) extends MoveFunction {
  override def apply(pres: Presentation) = pres.lftmult(k,l)
}

case class Conj(k: Int, l: Int) extends MoveFunction {
  override def apply(pres: Presentation) = pres.conj(k,l)
}

object MoveFunction {
  def toFunction(mf: MoveFunction) = mf.toFunc
}

case class Moves(moves: List[MoveFunction]) {
  def reduce: Presentation => Presentation = {
    if(moves.isEmpty)
      Presentation.id(_)
    else {
      val f = ((x: Presentation => Presentation, y: Presentation => Presentation) => x compose y)
      (moves map ((m: MoveFunction) => m.toFunc)) reduce f
    }
  }

  def apply(pres: Presentation) = this.reduce(pres)
  def apply(that: Moves) = this compose that
  def apply(that: Presentation => Presentation) = this.reduce compose that
  def apply(that: MoveFunction) = this compose that

  def length = moves.length

  def compose(that: Moves): Moves = {
    Moves(this.moves ++ that.moves)
  }

  def actOnTriv(rank: Int) = this(Presentation.trivial(rank))
}

object Moves {
  implicit def toMoves(move: MoveFunction): Moves = Moves(List(move))
}
