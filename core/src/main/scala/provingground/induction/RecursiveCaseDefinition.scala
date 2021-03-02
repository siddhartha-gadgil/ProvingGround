package provingground.induction

import shapeless._


import provingground._, HoTT._

/**
  * recursively defined function, to be built by mixing in cases,
  * defaults to a formal application by itself
  */
trait RecursiveDefinition[H <: Term with Subs[H], C <: Term with Subs[C]]
    extends RecFunc[H, C] { self =>

  def fromData(data: Vector[Term]): RecursiveDefinition[H, C]

  /**
    * the optional recursive definition if a case is matched
    */
  def caseFn(f: => Func[H, C])(arg: H): Option[C]

  def dataSubs(
      that: RecursiveDefinition[H, C],
      x: Term,
      y: Term
  ): RecursiveDefinition[H, C]

  def act(arg: H) = {
    caseFn(self)(arg) getOrElse codom.symbObj(ApplnSym(self, arg))
  }

  def subs(x: Term, y: Term): RecursiveDefinition[H, C]

  def rebuilt: RecursiveDefinition[H, C]
}

object RecursiveDefinition {
  def rebuild[U <: Term with Subs[Term]](t: U): U = t match {
    case lf: LambdaFixed[u, v] =>
      lmbda(rebuild(lf.variable))(rebuild(lf.value)).asInstanceOf[U]
    case lt: LambdaLike[u, v] =>
      lambda(rebuild(lt.variable))(rebuild(lt.value)).asInstanceOf[U]
    case FormalAppln(func: FuncLike[u, v], arg) =>
      (rebuild(func)(rebuild(arg).asInstanceOf[u])).asInstanceOf[U]

    case term => term
  }

  /**
    * empty [[RecursiveDefinition]], always a formal application
    */
  case class Empty[H <: Term with Subs[H], C <: Term with Subs[C]](
      dom: Typ[H],
      codom: Typ[C]
  ) extends RecursiveDefinition[H, C] {
    val defnData = Vector()

    val intros: Vector[HoTT.Term] = Vector()

    def fromData(data: Vector[Term]) = this

    val typ = dom ->: codom

    def subs(x: Term, y: Term) = Empty(dom.replace(x, y), codom.replace(x, y))

    def dataSubs(
        that: RecursiveDefinition[H, C],
        x: Term,
        y: Term
    ): RecursiveDefinition[H, C] = that

    def newobj = {
      val newdom = dom.newobj
      Empty(newdom, codom.replace(dom, newdom))
    }

    def caseFn(f: => Func[H, C])(arg: H): Option[C] = None

    def rebuilt = this
  }

  /**
    * an additional case for a [[RecursiveDefinition]], depending on definition data `data`
    */
  case class DataCons[H <: Term with Subs[H], C <: Term with Subs[C], D <: Term with Subs[
    D
  ]](
      data: D,
      defn: D => Func[H, C] => H => Option[C],
      cons: Term,
      tail: RecursiveDefinition[H, C],
      replacement: Term => Term => Typ[C] => Option[DataCons[H, C, D]] =
        (_: Term) => (_: Term) => (_: Typ[C]) => None
  ) extends RecursiveDefinition[H, C] {
    val dom = tail.dom

    val codom = tail.codom

    val typ = dom ->: codom

    val defnData = data +: tail.defnData

    lazy val intros: Vector[HoTT.Term] = cons +: tail.intros

    def fromData(data: Vector[Term]) =
      DataCons(
        data.head.asInstanceOf[D],
        defn,
        cons,
        tail.fromData(data.tail),
        replacement
      )

    def dataSubs(
        that: RecursiveDefinition[H, C],
        x: Term,
        y: Term
    ): RecursiveDefinition[H, C] = that match {
      case dc: DataCons[H, C, D] =>
        DataCons(
          data.replace(x, y),
          dc.defn,
          cons,
          tail.dataSubs(dc.tail, x, y),
          dc.replacement
        )
      case fn => fn
    }

    def newobj = {
      // println("Calling new object")
      DataCons(data.newobj, defn, cons, tail, replacement)
    }

    def subs(x: Term, y: Term) = {
      val newData = data.replace(x, y)
      replacement(x)(y)(codom)
        .map(dataSubs(_, x, y))
        .getOrElse(DataCons(newData, defn, cons.replace(x, y), tail.subs(x, y), replacement))
    }

    def caseFn(f: => Func[H, C])(arg: H): Option[C] =
      defn(data)(f)(arg) orElse (tail.caseFn(f)(arg))

    def rebuilt = DataCons(rebuild(data), defn, cons, tail.rebuilt, replacement)

  }

}

import TermList.TermListOp

/**
  * indexed version of [[RecursiveDefinition]]
  */
sealed abstract class IndexedRecursiveDefinition[H <: Term with Subs[H], F <: Term with Subs[
  F
], C <: Term with Subs[C], Index <: HList: TermList, IF <: Term with Subs[IF], IDF <: Term with Subs[
  IDF
], IDFT <: Term with Subs[IDFT]] {
  self =>
  val family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]

  val W: F

  val X: Typ[C]

  val intros: Vector[Term]

  val defnData: Vector[Term]

  def fromData(
      data: Vector[Term]
  ): IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT]

  def caseFn(f: => IF)(arg: H): Option[C]

  case class Funcs(ind: Index) extends IndRecFunc[H, C, F] { fself =>
    val dom = family.pattern.typ(W, ind)

    val domW = self.W

    val intros: Vector[HoTT.Term] = self.intros

    val index = ind.terms

    val codom = X

    val defnData = self.defnData

    def fromData(data: Vector[Term]) = self.fromData(data).Funcs(ind)

    val typ = dom ->: codom

    lazy val outer = self

    // override lazy val hashCode = (outer, ind).hashCode
    //
    // override def equals(that: Any) = that match {
    //   case fn: IndexedRecursiveDefinition[a, b, c, d, e, f, g]#Funcs =>
    //     (outer, ind) == (fn.outer, fn.ind)
    //   case _ => false
    // }

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)"
      ) // should not be called

    def act(arg: H) =
      caseFn(iterFunc)(arg) getOrElse (codom.symbObj(ApplnSym(fself, arg)))

    def subs(x: Term, y: Term) = self.subs(x, y).Funcs(ind.subst(x, y))

  }

  lazy val iterFunc = family.iterFunc(Funcs)

  def subs(
      x: Term,
      y: Term
  ): IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT]

  def dataSubs(
      that: IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT],
      x: Term,
      y: Term
  ): IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT]
}

object IndexedRecursiveDefinition {
  case class Empty[H <: Term with Subs[H], F <: Term with Subs[F], C <: Term with Subs[
    C
  ], Index <: HList: TermList, IF <: Term with Subs[IF], IDF <: Term with Subs[
    IDF
  ], IDFT <: Term with Subs[IDFT]](
      W: F,
      X: Typ[C],
      family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]
  ) extends IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT] {

    val defnData = Vector()

    val intros = Vector()

    def fromData(data: Vector[Term]) = this

    def caseFn(f: => IF)(arg: H): Option[C] = None

    def subs(x: Term, y: Term) =
      Empty(W.replace(x, y), X.replace(x, y), family.subs(x, y))

    def dataSubs(
        that: IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT],
        x: Term,
        y: Term
    ) = that
  }

  case class DataCons[H <: Term with Subs[H], F <: Term with Subs[F], C <: Term with Subs[
    C
  ], Index <: HList: TermList, IF <: Term with Subs[IF], IDF <: Term with Subs[
    IDF
  ], IDFT <: Term with Subs[IDFT], D <: Term with Subs[D]](
      data: D,
      cons: Term,
      defn: D => IF => H => Option[C],
      tail: IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT],
      replacement: Term => Term => Typ[C] => Option[
        DataCons[H, F, C, Index, IF, IDF, IDFT, D]
      ] = (_: Term) => (_: Term) => (_: Typ[C]) => None
  ) extends IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT] {
    val family = tail.family

    val defnData = data +: tail.defnData

    val intros = cons +: tail.intros

    def fromData(data: Vector[Term]) =
      DataCons(
        data.head.asInstanceOf[D],
        cons,
        defn,
        tail.fromData(data.tail),
        replacement
      )

    val W = tail.W

    val X = tail.X

    def caseFn(f: => IF)(arg: H): Option[C] =
      defn(data)(f)(arg) orElse (tail.caseFn(f)(arg))

    def subs(x: Term, y: Term) = {
      val newData = data.replace(x, y)
      replacement(x)(y)(X)
        .map(dataSubs(_, x, y))
        .getOrElse(
          DataCons(
            newData,
            cons.replace(x, y),
            defn,
            tail.subs(x, y),
            replacement
          )
        )
    }

    def dataSubs(
        that: IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT],
        x: Term,
        y: Term
    ) =
      that match {
        case dc: DataCons[H, F, C, Index, IF, IDF, IDFT, D] =>
          DataCons(
            data.replace(x, y),
            cons,
            dc.defn,
            tail.dataSubs(dc.tail, x, y),
            dc.replacement
          )
        case fn => fn
      }
  }
}
