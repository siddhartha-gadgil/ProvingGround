package provingground

import HoTT._

trait InductiveCaseDefinition[H <: Term with Subs[H], C <: Term with Subs[C]]
    extends FuncLike[H, C] { self =>
  def caseFn(f: => FuncLike[H, C])(arg: H): Option[C]

  def act(arg: H) = {
    caseFn(self)(arg) getOrElse depcodom(arg).symbObj(ApplnSym(self, arg))
  }

  def subs(x: Term, y: Term): InductiveCaseDefinition[H, C]
}

object InductiveCaseDefinition {
  case class Empty[H <: Term with Subs[H], C <: Term with Subs[C]](
      fibre: Func[H, Typ[C]]
  )
      extends InductiveCaseDefinition[H, C] {
    val typ = PiTyp(fibre)

    val depcodom = fibre

    val dom = fibre.dom

    def subs(x: Term, y: Term) = Empty(fibre.replace(x, y))

    def newobj = Empty(fibre.newobj)

    def caseFn(f: => FuncLike[H, C])(arg: H): Option[C] = None
  }

  case class DataCons[
      H <: Term with Subs[H], C <: Term with Subs[C], D <: Term with Subs[D]](
      data: D,
      defn: D => FuncLike[H, C] => H => Option[C],
      tail: InductiveCaseDefinition[H, C]
  )
      extends InductiveCaseDefinition[H, C] {
    val typ = tail.typ

    val dom = tail.dom

    val depcodom = tail.depcodom

    def newobj = DataCons(data.newobj, defn, tail)

    def subs(x: Term, y: Term) =
      DataCons(data.replace(x, y), defn, tail.subs(x, y))

    def caseFn(f: => FuncLike[H, C])(arg: H): Option[C] =
      defn(data)(f)(arg) orElse (tail.caseFn(f)(arg))
  }
}
