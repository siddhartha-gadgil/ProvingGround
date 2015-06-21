package provingground.andrewscurtis

import provingground.andrewscurtis.FreeGroups._
import scala.language.implicitConversions
import Moves._

case class Moves(moves: List[Presentation => Presentation]) {
  def reduce: Presentation => Presentation = {
    if(moves.isEmpty)
      Presentation.id(_)
    else {
      val f = ((x: Presentation => Presentation, y: Presentation => Presentation) => x compose y)
      moves reduce f
    }
  }

  def apply(pres: Presentation) = this.reduce(pres)
  def apply(that: Moves) = this compose that
  def apply(that: Presentation => Presentation) = this compose toMoves(that)

  def length = moves.length

  def compose(that: Moves): Moves = {
    Moves(this.moves ++ that.moves)
  }

  def actOnTriv(rank: Int) = this(Presentation.trivial(rank))
}

object Moves {
  implicit def toMoves(move: Presentation => Presentation) = Moves(List(move))
}
