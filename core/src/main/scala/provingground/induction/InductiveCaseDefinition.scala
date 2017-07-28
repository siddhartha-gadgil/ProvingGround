package provingground.induction

import provingground._, HoTT._

import shapeless._

/**
  * inductively defined dependent function, to be built by mixing in cases,
  * defaults to a formal application by itself
  */
trait InductiveDefinition[H <: Term with Subs[H], C <: Term with Subs[C]]
    extends InducFuncLike[H, C] { self =>
  val fibre: Func[H, Typ[C]]

  def dataSubs(that: InductiveDefinition[H, C],
               x: Term,
               y: Term): InductiveDefinition[H, C]

  def caseFn(f: => FuncLike[H, C])(arg: H): Option[C]

  def act(arg: H) = {
    caseFn(self)(arg) getOrElse depcodom(arg).symbObj(ApplnSym(self, arg))
  }

  def subs(x: Term, y: Term): InductiveDefinition[H, C]
}

object InductiveDefinition {

  /**
    * empty [[InductiveDefinition]], always a formal application.
    */
  case class Empty[H <: Term with Subs[H], C <: Term with Subs[C]](
      fibre: Func[H, Typ[C]]
  ) extends InductiveDefinition[H, C] {
    val typ = PiDefn(fibre)

    val defnData = Vector()

    def dataSubs(that: InductiveDefinition[H, C],
                 x: Term,
                 y: Term): InductiveDefinition[H, C] = that

    val depcodom = fibre

    val dom = fibre.dom

    def subs(x: Term, y: Term) = Empty(fibre.replace(x, y))

    def newobj = Empty(fibre.newobj)

    def caseFn(f: => FuncLike[H, C])(arg: H): Option[C] = None
  }

  /**
    * an additional case for an [[InductiveDefinition]], depending on definition data `data`
    */
  case class DataCons[H <: Term with Subs[H],
                      C <: Term with Subs[C],
                      D <: Term with Subs[D]](
      data: D,
      defn: D => FuncLike[H, C] => H => Option[C],
      tail: InductiveDefinition[H, C],
      replacement: Term => Term => Func[H, Typ[C]] => Option[DataCons[H, C, D]] =
        (_: Term) => (_: Term) => (_: Func[H, Typ[C]]) => None
  ) extends InductiveDefinition[H, C] {
    val typ = tail.typ

    val fibre = tail.fibre

    val dom = tail.dom

    val depcodom = tail.depcodom

    val defnData = data +: tail.defnData

    def dataSubs(that: InductiveDefinition[H, C],
                 x: Term,
                 y: Term): InductiveDefinition[H, C] =
      that match {
        case dc: DataCons[H, C, D] =>
          DataCons(data.replace(x, y),
                   dc.defn,
                   tail.dataSubs(dc.tail, x, y),
                   dc.replacement)
        case fn => fn
      }

    def newobj = DataCons(data.newobj, defn, tail, replacement)

    def subs(x: Term, y: Term) =
      replacement(x)(y)(fibre)
        .map(dataSubs(_, x, y))
        .getOrElse(
          DataCons(data.replace(x, y), defn, tail.subs(x, y), replacement))

    def caseFn(f: => FuncLike[H, C])(arg: H): Option[C] =
      defn(data)(f)(arg) orElse (tail.caseFn(f)(arg))
  }
}

import TermList.TermListOp

/**
  * indexed version of [[InductiveDefinition]]
  */
abstract class IndexedInductiveDefinition[H <: Term with Subs[H],
                                          F <: Term with Subs[F],
                                          C <: Term with Subs[C],
                                          Index <: HList: TermList,
                                          IF <: Term with Subs[IF],
                                          IDF <: Term with Subs[IDF],
                                          IDFT <: Term with Subs[IDFT]] {
  self =>
  val family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]

  val defnData: Vector[Term]

  val W: F

  val Xs: IDFT

  def caseFn(f: => IDF)(arg: H): Option[C]

  case class Funcs(ind: Index) extends IndInducFuncLike[H, C, F, IDFT] {
    fself =>
    val dom = family.pattern.typ(W, ind)

    val domW  = self.W
    val codXs = self.Xs

    val index = ind.terms

    val defnData = self.defnData

    val fibre = family.typRestrict(Xs, ind)

    lazy val outer = self

    val depcodom = fibre

    val typ = PiDefn(fibre)

    def newobj                 = ??? // should not be called
    override lazy val hashCode = (outer, ind).hashCode

    override def equals(that: Any) = that match {
      case fn: IndexedInductiveDefinition[a, b, c, d, e, f, g]#Funcs =>
        (outer, ind) == (fn.outer, fn.ind)
      case _ => false
    }

    def act(arg: H) =
      caseFn(iterDepFunc)(arg) getOrElse
        (depcodom(arg).symbObj(ApplnSym(fself, arg)))

    def subs(x: Term, y: Term) = self.subs(x, y).Funcs(ind.subst(x, y))
  }

  lazy val iterDepFunc = family.iterDepFunc(Funcs)

  def subs(x: Term,
           y: Term): IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT]

  def dataSubs(
      that: IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT],
      x: Term,
      y: Term): IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT]
}

object IndexedInductiveDefinition {
  case class Empty[H <: Term with Subs[H],
                   F <: Term with Subs[F],
                   C <: Term with Subs[C],
                   Index <: HList: TermList,
                   IF <: Term with Subs[IF],
                   IDF <: Term with Subs[IDF],
                   IDFT <: Term with Subs[IDFT]](
      W: F,
      Xs: IDFT,
      family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]
  ) extends IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT] {

    val defnData = Vector()

    def caseFn(f: => IDF)(arg: H): Option[C] = None

    def subs(x: Term, y: Term) =
      Empty(W.replace(x, y), Xs.replace(x, y), family.subs(x, y))

    def dataSubs(
        that: IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT],
        x: Term,
        y: Term) = that
  }

  case class DataCons[H <: Term with Subs[H],
                      F <: Term with Subs[F],
                      C <: Term with Subs[C],
                      Index <: HList: TermList,
                      IF <: Term with Subs[IF],
                      IDF <: Term with Subs[IDF],
                      IDFT <: Term with Subs[IDFT],
                      D <: Term with Subs[D]](
      data: D,
      defn: D => IDF => H => Option[C],
      tail: IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT],
      replacement: Term => Term => IDFT => Option[
        DataCons[H, F, C, Index, IF, IDF, IDFT, D]] = (_: Term) =>
        (_: Term) => (_: IDFT) => None
  ) extends IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT] {
    val family = tail.family

    val W = tail.W

    val defnData = data +: tail.defnData

    val Xs = tail.Xs

    def caseFn(f: => IDF)(arg: H): Option[C] =
      defn(data)(f)(arg) orElse (tail.caseFn(f)(arg))

    def subs(x: Term, y: Term) =
      replacement(x)(y)(Xs)
        .map(dataSubs(_, x, y))
        .getOrElse(
          DataCons(data.replace(x, y), defn, tail.subs(x, y), replacement))

    def dataSubs(
        that: IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT],
        x: Term,
        y: Term) = that match {
      case dc: DataCons[H, F, C, Index, IF, IDF, IDFT, D] =>
        DataCons(data.replace(x, y),
                 dc.defn,
                 tail.dataSubs(dc.tail, x, y),
                 dc.replacement)
      case fn => fn
    }
  }
}
