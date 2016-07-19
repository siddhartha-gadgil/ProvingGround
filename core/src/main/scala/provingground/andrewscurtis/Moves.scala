package provingground.andrewscurtis

import provingground.andrewscurtis.FreeGroups._
//import provingground.Collections._
import scala.language.implicitConversions
import Moves._
//import AtomicMove._
import provingground._
//import FiniteDistribution._
import FiniteDistributionLearner._

object AtomicMove {
  def actOnFDVertices(
      mf: AtomicMove,
      fdVertices: FiniteDistribution[Moves]): FiniteDistribution[Moves] =
    mf(fdVertices)
  def actOnMoves(mf: AtomicMove): Moves => Option[Moves] = mf.actOnMoves(_)
  def apply(w: String): AtomicMove = fromString(w).get
  def fromString(w: String): Option[AtomicMove] = {
    // Regular expressions to match to various case classes
    // Won't work in conj if there are more than 26 generators
    val id = """^id$""".r
    val inv = """^[0-9]+!$""".r
    val lftmult = """^[0-9]+->[0-9]+$""".r
    val rtmult = """^[0-9]+<-[0-9]+$""".r
    val conj = """^[0-9]+\^[a-z]!?$""".r
    val numbers = """[0-9]+""".r
    val letters = """[a-z]!?$""".r

    if (id.findFirstIn(w).isDefined) {
      Some(Id)
    } else if (inv.findFirstIn(w).isDefined) {
      val rel = (w.takeWhile(_ != '!')).toInt
      Some(Inv(rel))
    } else if (lftmult.findFirstIn(w).isDefined) {
      val nums = numbers.findAllMatchIn(w).toList
      val l = nums(0).toString.toInt
      val k = nums(1).toString.toInt
      Some(LftMult(k, l))
    } else if (rtmult.findFirstIn(w).isDefined) {
      val nums = numbers.findAllMatchIn(w).toList
      val k = nums(0).toString.toInt
      val l = nums(1).toString.toInt
      Some(RtMult(k, l))
    } else if (conj.findFirstIn(w).isDefined) {
      val letter = letters.findFirstIn(w).toList(0).toString.apply(0)
      val multiplier =
        if (w.contains('!'))
          -1
        else
          1
      val generator = (letter.toInt - 'a'.toInt + 1) * multiplier
      val relation = numbers.findAllMatchIn(w).toList(0).toString.toInt
      Some(Conj(relation, generator))
    } else
      None
  }
}

sealed trait AtomicMove extends (Moves => Option[Moves]) {
  def apply(pres: Presentation): Option[Presentation]

  def apply(opPres: Option[Presentation]): Option[Presentation] = {
    opPres match {
      case Some(pres) => this.apply(pres)
      case None => None
    }
  }

  def apply(moves: Moves): Option[Moves] = this.actOnMoves(moves)

  def apply(fdVertices: FiniteDistribution[Moves]): FiniteDistribution[Moves] = {
    (fdVertices mapOpt ((mv: Moves) => this(mv))).flatten
  }

  def actOnMoves(moves: Moves): Option[Moves] =
    Some(toMoves(this) compose moves)

  def movesDF: DiffbleFunction[
      FiniteDistribution[Moves], FiniteDistribution[Moves]] =
    MoveFn(actOnMoves)

  def actOnPres(fdPres: FiniteDistribution[Presentation])
    : FiniteDistribution[Presentation] = {
    (fdPres mapOpt ((pres: Presentation) => this(pres))).flatten
  }

  def compose(mf: AtomicMove): Moves = {
    toMoves(this) compose toMoves(mf)
  }

  def toFunc: Presentation => Option[Presentation] =
    (pres: Presentation) => this(pres)

  def toPlainString: String = {
    this match {
      case Id => "id"
      case Inv(k) => s"$k!"
      case RtMult(k, l) => s"$k<-$l"
      case LftMult(k, l) => s"$l->$k"
      case Conj(k, l) =>
        if (l > 0) {
          val letter = (l + 'a'.toInt - 1).toChar
          s"$k^$letter"
        } else {
          val letter = ((-1) * l + 'a'.toInt - 1).toChar.toString + "!"
          s"$k^$letter"
        }
      case _ => "function1"
    }
  }

  def toLatex = {
    this match {
      case Id => s"$$r \\mapsto r$$"
      case Inv(k) => s"$$r_{$k} \\mapsto \\bar{r_$k}$$"
      case RtMult(k, l) =>
        if (k < l)
          s"$$(r_{$k}, r_{$l}) \\mapsto (r_{$k}r_{$l}, r_{$l})$$"
        else
          s"$$(r_{$l}, r_{$k}) \\mapsto (r_{$l}, r_{$k}r_{$l})$$"

      case LftMult(k, l) =>
        if (k < l)
          s"$$(r_{$k}, r_{$l}) \\mapsto (r_{$l}r_{$k}, r_{$l})$$"
        else
          s"$$(r_{$l}, r_{$k}) \\mapsto (r_{$l}, r_{$l}r_{$k})$$"
      case Conj(k, l) =>
        s"$$r_{$k} \\mapsto \\bar{\\alpha_{$l}}r_{$k}\\alpha_{$l}$$"
      case _ => "function1"
    }
  }
  /*
  def toUnicode = {
    val create_subscript = ((x: Char) =>
                                        x match {
                                        case '-' => 0x208b.toChar
                                        case num => (num.toString.toInt + 0x2080).toChar
                                        }
                           )
    val int_to_sub = ((x: Int) => x.toString map create_subscript)
    this match {
      case Id() => "r \u2192 r"
      case Inv(k) =>
        val sub = int_to_sub(k)
        s"r$sub \u2192 \u203er$sub"
      case _ => "function1"
    }
  }
   */
  override def toString = toPlainString
}

case object Id extends AtomicMove {
  def apply(pres: Presentation) = Some(pres)
//  override def actOnMoves(moves: Moves) = Some(moves)

  override def movesDF: DiffbleFunction[
      FiniteDistribution[Moves], FiniteDistribution[Moves]] =
    DiffbleFunction.Id[FiniteDistribution[Moves]]
}

case class Inv(k: Int) extends AtomicMove {
  def apply(pres: Presentation): Option[Presentation] = {
    if (k >= 0 && k < pres.sz)
      Some(pres.inv(k))
    else
      None
  }
}

case class RtMult(k: Int, l: Int) extends AtomicMove {
  def apply(pres: Presentation): Option[Presentation] = {
    if (k >= 0 && l >= 0 && k < pres.sz && l < pres.sz)
      Some(pres.rtmult(k, l))
    else
      None
  }
}

case class RtMultInv(k: Int, l: Int) extends AtomicMove {
  def apply(pres: Presentation): Option[Presentation] = {
    if (k >= 0 && l >= 0 && k < pres.sz && l < pres.sz)
      Some(pres.rtmultinv(k, l))
    else
      None
  }
}

case class LftMult(k: Int, l: Int) extends AtomicMove {
  def apply(pres: Presentation): Option[Presentation] = {
    if (k >= 0 && l >= 0 && k < pres.sz && l < pres.sz)
      Some(pres.lftmult(k, l))
    else
      None
  }
}

case class LftMultInv(k: Int, l: Int) extends AtomicMove {
  def apply(pres: Presentation): Option[Presentation] = {
    if (k >= 0 && l >= 0 && k < pres.sz && l < pres.sz)
      Some(pres.lftmult(k, l))
    else
      None
  }
}

case class Conj(k: Int, l: Int) extends AtomicMove {
  def apply(pres: Presentation): Option[Presentation] = {
    if (k >= 0 && math.abs(l) > 0 && k < pres.sz && math.abs(l) <= pres.rank)
      Some(pres.conj(k, l))
    else
      None
  }
}

case class Transpose(k: Int, l: Int) extends AtomicMove {
  def apply(pres: Presentation): Option[Presentation] = {
    if (k >= 0 && math.abs(l) > 0 && k < pres.sz && math.abs(l) <= pres.rank)
      Some(pres.transpose(k, l))
    else
      None
  }
}

case class Moves(moves: List[AtomicMove]) extends AnyVal {
  /*
  def reduce: Presentation => Option[Presentation] = {
    if(moves.isEmpty)
      (pres: Presentation) => Some(pres)
    else {
      val f = (x: Presentation => Option[Presentation], y: Presentation => Option[Presentation]) => liftOption(x) compose y
      (moves map ((mf: AtomicMove) => mf.toFunc)) reduce f
    }
  }
   */
  def reduce: Presentation => Option[Presentation] = (pres) => {
    def act(mv: AtomicMove, op: Option[Presentation]) = {
      op flatMap ((p) => mv(p))
    }
    (moves :\ (Some(pres): Option[Presentation]))(act)
  }

  def apply(pres: Presentation) = this.reduce(pres)
  def apply(that: Moves) = this compose that
  def apply(that: Presentation => Option[Presentation]) =
    liftOption(this.reduce) compose that
  def apply(that: AtomicMove) = this compose that

  def length = moves.length

  def compose(that: Moves): Moves = {
    Moves(this.moves ++ that.moves)
  }

  def actOnTriv(rank: Int) = this(Presentation.trivial(rank))

  def idLast =
    Moves(moves.filter(_ != Id) ++ moves.filter(_ == Id))
}

object Moves {
  def empty = Moves(List.empty)

  def apply(ws: String*) = fromString(ws).get

  def fromString(ws: Seq[String]): Option[Moves] = {
    val atomic_moves = ws.toList map (AtomicMove.fromString(_))
    if (atomic_moves.contains(None))
      None
    else
      Some(Moves(for { Some(i) <- atomic_moves } yield i))
  }

  implicit def toMoves(move: AtomicMove): Moves = Moves(List(move))

  def liftOption[A](f: A => Option[A]): Option[A] => Option[A] = {
    def lifted_f(a: Option[A]): Option[A] = {
      if (a.isDefined)
        f(a.get)
      else
        None
    }
    lifted_f
  }

  def liftResult[A](f: A => A): A => Option[A] = { (a: A) =>
    Some(f(a))
  }

  def actOnTriv(rank: Int)(mvs: Moves) = mvs.actOnTriv(rank)
}
