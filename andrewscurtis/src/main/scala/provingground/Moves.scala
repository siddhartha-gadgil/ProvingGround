package provingground.andrewscurtis

import provingground.andrewscurtis.FreeGroups._
import provingground.Collections._
import scala.language.implicitConversions
import Moves._

sealed trait MoveFunction {
  def apply(pres: Presentation): Option[Presentation]
  
  def apply(opPres: Option[Presentation]): Option[Presentation] = {
    opPres match {
      case Some(pres) => this.apply(pres)
      case None => None
    }
  }
  
  def apply(moves: Moves): Moves = toMoves(this) compose moves
  
  def compose(mf: MoveFunction): Moves = {
    toMoves(this) compose toMoves(mf)
  }

  def toFunc: Presentation => Option[Presentation] = (pres: Presentation) => this(pres)
}

case class Id() extends MoveFunction {
  override def apply(pres: Presentation) = Some(pres)
}

case class Inv(k: Int) extends MoveFunction {
  override def apply(pres: Presentation): Option[Presentation] = {
    if(k>=0 && k<pres.sz)
      Some(pres.inv(k))
    else
      None
  }
}

case class RtMult(k: Int, l: Int) extends MoveFunction {
  override def apply(pres: Presentation): Option[Presentation] = {
    if(k>=0 && l>=0 && k<pres.sz && l<pres.sz)
      Some(pres.rtmult(k,l))
    else
      None
  }
}

case class LftMult(k: Int, l: Int) extends MoveFunction {
  override def apply(pres: Presentation): Option[Presentation] = {
    if(k>=0 && l>=0 && k<pres.sz && l<pres.sz)
      Some(pres.lftmult(k,l))
    else
      None
  }
}

case class Conj(k: Int, l: Int) extends MoveFunction {
  override def apply(pres: Presentation): Option[Presentation] = {
    if(k>=0 && l>0 && k<pres.sz && l<=pres.rank)
      Some(pres.conj(k,l))
    else
      None
  }
}

case class Moves(moves: List[MoveFunction]) {
  def reduce: Presentation => Option[Presentation] = {
    if(moves.isEmpty)
      (pres: Presentation) => Some(pres)
    else {
      val f = (x: Presentation => Option[Presentation], y: Presentation => Option[Presentation]) => liftOption(x) compose y
      (moves map ((mf: MoveFunction) => mf.toFunc)) reduce f
    }
  }

  def apply(pres: Presentation) = this.reduce(pres)
  def apply(that: Moves) = this compose that
  def apply(that: Presentation => Option[Presentation]) = liftOption(this.reduce) compose that
  def apply(that: MoveFunction) = this compose that

  def length = moves.length

  def compose(that: Moves): Moves = {
    Moves(this.moves ++ that.moves)
  }

  def actOnTriv(rank: Int) = this(Presentation.trivial(rank))
}

object Moves {
  implicit def toMoves(move: MoveFunction): Moves = Moves(List(move))

  def liftOption[A](f: A => Option[A]): Option[A] => Option[A] = {
    def lifted_f(a: Option[A]): Option[A] = {
      if(a.isDefined)
        f(a.get)
      else
        None
    }
    lifted_f
  }

  def liftResult[A](f: A => A): A => Option[A] = {
    (a: A) => Some(f(a))
  }
}
