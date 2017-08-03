package provingground.functionfinder
import provingground._, HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{
  Try => UnivTry,
  Function => FunctionUniv,
  _
}
//import provingground.ScalaUniv._

object ListType {
  case class ListTyp[U <: Term with Subs[U]](elemTyp: Typ[U]) extends SmallTyp

  case class ListTerm[U <: Term with Subs[U]](value: List[U], elemTyp: Typ[U])
    extends ConstTerm[List[U]] {
    val typ = ListTyp(elemTyp)
  }

  case class ListRep[U <: Term with Subs[U]](elemTyp: Typ[U])
    extends ScalaRep[Term, List[U]] {
    val typ = ListTyp(elemTyp)

    def apply(l: List[U]) = ListTerm(l, elemTyp)

    def unapply(u: Term) = u match {
      case ListTerm(l, `elemTyp`) => Some(l.asInstanceOf[List[U]])
      case _ => None
    }

    def subs(x: Term, y: Term) = ListRep(elemTyp.subs(x, y))
  }

  def foldFunction[U <: Term with Subs[U], V <: Term with Subs[V]](
    u: Typ[U],
    v: Typ[V]) = {
    val rep = ListRep(u) -->: v -->: (u -->: v -->: v) -->: v
    val fld = (l: List[U]) =>
      (init: V) =>
        (op: U => V => V) => {
          def cop(u: U, v: V) = op(u)(v)
          (l :\ init)(cop)
        }
    rep(fld)
  }

  def lmapFunc[U <: Term with Subs[U], V <: Term with Subs[V]](
    u: Typ[U],
    v: Typ[V]) = {
    val rep = (u -->: v) -->: ListRep(u) -->: ListRep(v)
    rep((f: U => V) => (l: List[U]) => l map (f))
  }

  private val u = "u" :: Type
  private val v = "v" :: Type

  lazy val lmap = lambda(u)(lambda(v)(lmapFunc(u, v)))
  //depFunc(Type, (u: Typ[Term])=> depFunc(Type, (v: Typ[Term]) => lmapFunc(u, v)))

  lazy val foldLeft = lambda(u)(lambda(v)(foldFunction(u, v)))
  //depFunc(Type, (u: Typ[Term]) => depFunc(Type, (v: Typ[Term]) => foldFunction(u, v)))

  def headOptFn[U <: Term with Subs[U]](typ: Typ[U]) = {
    val rep = ListRep(typ) -->: IdRep(typ)
    rep.opt((l: List[U]) => l.headOption)
  }

  def tailFn[U <: Term with Subs[U]](typ: Typ[U]) = {
    val rep = ListRep(typ) -->: ListRep(typ)
    rep((l: List[U]) => l drop 1)
  }

  lazy val headOpt = lambda(u)(headOptFn(u))

  lazy val tail = lambda(u)(tailFn(u))
}
