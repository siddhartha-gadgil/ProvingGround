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
}

import Subst.SubstOp

abstract class IndexedRecursiveDefinition[H <: Term with Subs[H],
                                          F <: Term with Subs[F],
                                          C <: Term with Subs[C],
                                          Index: Subst,
                                          IF <: Term with Subs[IF],
                                          IDF <: Term with Subs[IDF],
                                          IDFT <: Term with Subs[IDFT]] {
  self =>
  val family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]

  val W: F

  val X: Typ[C]

  def caseFn(f: => IF)(arg: H): Option[C]

  case class Funcs(ind: Index) extends Func[H, C] { fself =>
    val dom = family.pattern.typ(W, ind)

    val codom = X

    val typ = dom ->: codom

    def newobj = fself

    def act(arg: H) =
      caseFn(iterFunc)(arg) getOrElse (codom.symbObj(ApplnSym(fself, arg)))

    def subs(x: Term, y: Term) = self.subs(x, y).Funcs(ind.subst(x, y))
  }

  lazy val iterFunc = family.iterFunc(Funcs)

  def subs(x: Term,
           y: Term): IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT]
}

object IndexedRecursiveDefinition {
  case class Empty[H <: Term with Subs[H],
                   F <: Term with Subs[F],
                   C <: Term with Subs[C],
                   Index: Subst,
                   IF <: Term with Subs[IF],
                   IDF <: Term with Subs[IDF],
                   IDFT <: Term with Subs[IDFT]](
      W: F,
      X: Typ[C],
      family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]
  )
      extends IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT] {

    def caseFn(f: => IF)(arg: H): Option[C] = None

    def subs(x: Term, y: Term) =
      Empty(W.replace(x, y), X.replace(x, y), family.subs(x, y))
  }

  case class DataCons[H <: Term with Subs[H],
                      F <: Term with Subs[F],
                      C <: Term with Subs[C],
                      Index: Subst,
                      IF <: Term with Subs[IF],
                      IDF <: Term with Subs[IDF],
                      IDFT <: Term with Subs[IDFT],
                      D <: Term with Subs[D]](
      data: D,
      defn: D => IF => H => Option[C],
      tail: IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT]
  )
      extends IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT] {
    val family = tail.family

    val W = tail.W

    val X = tail.X

    def caseFn(f: => IF)(arg: H): Option[C] =
      defn(data)(f)(arg) orElse (tail.caseFn(f)(arg))

    def subs(x: Term, y: Term) =
      DataCons(data.replace(x, y), defn, tail.subs(x, y))
  }
}
