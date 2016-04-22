package provingground

import HoTT._
//import scala.language.existentials

trait RecursiveCaseDefinition[H <: Term with Subs[H], C <: Term with Subs[C]] extends Func[H, C] { self =>
  def caseFn(f: => Func[H, C])(arg: H): Option[C]

  def act(arg: H) = {
    caseFn(self)(arg) getOrElse codom.symbObj(ApplnSym(self, arg))
  }

  def subs(x: Term, y: Term): RecursiveCaseDefinition[H, C]
}

object RecursiveCaseDefinition {
  case class Empty[H <: Term with Subs[H], C <: Term with Subs[C]](
    dom: Typ[H], codom: Typ[C]
  ) extends RecursiveCaseDefinition[H, C] {
    val typ = dom ->: codom

    def subs(x: Term, y: Term) = Empty(dom.replace(x, y), codom.replace(x, y))

    def newobj = Empty(dom.newobj, codom.newobj)

    def caseFn(f: => Func[H, C])(arg: H): Option[C] = None
  }

  case class DataCons[H <: Term with Subs[H], C <: Term with Subs[C], D <: Term with Subs[D]](
    data: D,
    defn: D => Func[H, C] => H => Option[C],
    tail: RecursiveCaseDefinition[H, C]
  ) extends RecursiveCaseDefinition[H, C] {
    val dom = tail.dom

    val codom = tail.codom

    val typ = dom ->: codom

    def newobj = DataCons(data.newobj, defn, tail)

    def subs(x: Term, y: Term) =
      DataCons(data.replace(x, y), defn, tail.subs(x, y))

    def caseFn(f: => Func[H, C])(arg: H): Option[C] =
      defn(data)(f)(arg) orElse (tail.caseFn(f)(arg))
  }

  def sym[C <: Term with Subs[C], H <: Term with Subs[H]](cons: Constructor[C, H]) = Constructor.Sym(cons)

  //for experimentation, should actually chain constructors.
  def constructorFunc[H <: Term with Subs[H], C <: Term with Subs[C]](
    cons: Constructor[C, H], X: Typ[C], tail: RecursiveCaseDefinition[H, C]
  ) =
    {
      val data: cons.pattern.RecDataType = cons.pattern.recDom(cons.W, X).symbObj(sym(cons))
      val defn = (d: cons.pattern.RecDataType) => (f: Func[H, C]) => cons.pattern.recDef(cons.cons, d, f)
      val fn: Func[H, C] = DataCons(data, defn, tail)
      lmbda(data)(fn)
    }

}

trait ConstructorSeq[C <: Term with Subs[C], H <: Term with Subs[H]] {
  val recCaseDefn: RecursiveCaseDefinition[H, C]

  val W: Typ[H]

  val X: Typ[C]

  type RecType <: Term with Subs[RecType]

  val rec : RecType

  def ::(head: Constructor[C, H]) = ConstructorSeq.Cons(head, this)
}

object ConstructorSeq {
  case class Empty[C <: Term with Subs[C], H <: Term with Subs[H]](W: Typ[H], X: Typ[C]) extends ConstructorSeq[C, H] {
    val recCaseDefn = RecursiveCaseDefinition.Empty(W, X)

    type RecType = Func[H, C]

    val rec : Func[H, C] = recCaseDefn
  }

  case class Cons[C <: Term with Subs[C], H <: Term with Subs[H]](
    cons: Constructor[C, H], tail: ConstructorSeq[C, H]
  ) extends ConstructorSeq[C, H] {

    val W = tail.W

    val X = tail.X

    val data: cons.pattern.RecDataType = cons.pattern.recDom(cons.W, X).symbObj(Constructor.Sym(cons))

    val defn = (d: cons.pattern.RecDataType) => (f: Func[H, C]) => cons.pattern.recDef(cons.cons, d, f)

    val recCaseDefn = RecursiveCaseDefinition.DataCons(data, defn, tail.recCaseDefn)

    type RecType = Func[cons.pattern.RecDataType, tail.RecType]

    val rec = lmbda(data)(tail.rec)
  }

  def fold[C<: Term with Subs[C], H<: Term with Subs[H]](
    W: Typ[H], X: Typ[C]): List[Constructor[C, H]] => ConstructorSeq[C, H] = {
      case List() => ConstructorSeq.Empty(W, X)
      case head :: tail => ConstructorSeq.Cons(head, fold(W, X)(tail))
    }

  def recFn[C<: Term with Subs[C], H<: Term with Subs[H]](
    W: Typ[H], X: Typ[C])(cs: List[Constructor[C, H]]) = fold(W, X)(cs).rec

}
