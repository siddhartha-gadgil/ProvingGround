package provingground.interface
import provingground._

// import ammonite.ops._
import scala.util._

import scala.concurrent._
import monix.execution.Scheduler.Implicits.global
import monix.eval._

import HoTT.{Name => _, _}

import trepplein._

// import cats.Eval

import LeanToTerm._

// import translation.FansiShow._

case class LeanToTermMonix(defnMap: Map[Name, Term],
                           termIndModMap: Map[Name, TermIndMod],
                           unparsed: Vector[Name]) { self =>
  type TaskParser = (Expr, Vector[Term]) => Task[Term]

  def defnOpt(exp: Expr) =
    exp match {
      case Const(name, _) => defnMap.get(name)
      case _              => None
    }

  object Predef {
    def unapply(exp: Expr): Option[Term] =
      (
        defnOpt(exp)
        // .orElse(recDefns(parse)(exp))
      )
  }

  object RecIterAp {
    def unapply(exp: Expr): Option[(Name, Vector[Expr])] = exp match {
      case Const(Name.Str(prefix, "rec"), _) => Some((prefix, Vector()))
      case App(func, arg) =>
        unapply(func).map { case (name, vec) => (name, vec :+ arg) }
      case _ => None
    }
  }

  val inPropFamily: Term => Boolean = {
    case FormalAppln(f, _) => inPropFamily(f)
    case s: Symbolic =>
      val name = trepplein.Name(s.name.toString.split('.'): _*)
      termIndModMap.get(name).map(_.isPropn).getOrElse(false)
    case _ => false
  }

  def applyFuncPropOpt(func: Term, arg: Term): Option[Term] =
    applyFuncOpt(func, arg).orElse {
      if (inPropFamily(arg.typ)) Some(func) else None
    }

}
