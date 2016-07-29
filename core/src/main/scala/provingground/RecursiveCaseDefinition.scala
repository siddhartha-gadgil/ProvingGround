package provingground

import HoTT._

trait RecursiveDefinition[H <: Term with Subs[H], C <: Term with Subs[C]]
    extends Func[H, C] { self =>
  def caseFn(f: => Func[H, C])(arg: H): Option[C]

  def act(arg: H) = {
    caseFn(self)(arg) getOrElse codom.symbObj(ApplnSym(self, arg))
  }

  def subs(x: Term, y: Term): RecursiveDefinition[H, C]
}

object RecursiveDefinition {
  case class Empty[H <: Term with Subs[H], C <: Term with Subs[C]](
      dom: Typ[H],
      codom: Typ[C]
  )
      extends RecursiveDefinition[H, C] {
    val typ = dom ->: codom

    def subs(x: Term, y: Term) = Empty(dom.replace(x, y), codom.replace(x, y))

    def newobj = Empty(dom.newobj, codom.newobj)

    def caseFn(f: => Func[H, C])(arg: H): Option[C] = None
  }

  case class DataCons[
      H <: Term with Subs[H], C <: Term with Subs[C], D <: Term with Subs[D]](
      data: D,
      defn: D => Func[H, C] => H => Option[C],
      tail: RecursiveDefinition[H, C]
  )
      extends RecursiveDefinition[H, C] {
    val dom = tail.dom

    val codom = tail.codom

    val typ = dom ->: codom

    def newobj = DataCons(data.newobj, defn, tail)

    def subs(x: Term, y: Term) =
      DataCons(data.replace(x, y), defn, tail.subs(x, y))

    def caseFn(f: => Func[H, C])(arg: H): Option[C] =
      defn(data)(f)(arg) orElse (tail.caseFn(f)(arg))
  }

//  def sym[C <: Term with Subs[C], H <: Term with Subs[H]](
//      cons: Constructor[C, H]) = Constructor.RecSym(cons)
//
//  //for experimentation, should actually chain constructors.
//  def constructorFunc[H <: Term with Subs[H], C <: Term with Subs[C]](
//      cons: Constructor[C, H],
//      X: Typ[C],
//      tail: RecursiveDefinition[H, C]
//  ) = {
//    val data: cons.pattern.RecDataType =
//      cons.pattern.recDataTyp(cons.W, X).symbObj(sym(cons))
//    val defn = (d: cons.pattern.RecDataType) =>
//      (f: Func[H, C]) => cons.pattern.recDefCase(cons.cons, d, f)
//    val fn: Func[H, C] = DataCons(data, defn, tail)
//    lmbda(data)(fn)
//  }
}
