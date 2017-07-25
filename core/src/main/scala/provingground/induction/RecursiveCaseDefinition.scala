package provingground.induction

import shapeless._

import HList._

import provingground._, HoTT._

/**
  * recursively defined function, to be built by mixing in cases,
  * defaults to a formal application by itself
  */
trait RecursiveDefinition[H <: Term with Subs[H], C <: Term with Subs[C]]
    extends RecFunc[H, C] { self =>

  /**
    * the optional recursive definition if a case is matched
    */
  def caseFn(f: => Func[H, C])(arg: H): Option[C]

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

    val typ = dom ->: codom

    def subs(x: Term, y: Term) = Empty(dom.replace(x, y), codom.replace(x, y))

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
  case class DataCons[H <: Term with Subs[H],
                      C <: Term with Subs[C],
                      D <: Term with Subs[D]](
      data: D,
      defn: D => Func[H, C] => H => Option[C],
      tail: RecursiveDefinition[H, C]
  ) extends RecursiveDefinition[H, C] {
    val dom = tail.dom

    val codom = tail.codom

    val typ = dom ->: codom

    val defnData = data +: tail.defnData

    def newobj = {
      // println("Calling new object")
      DataCons(data.newobj, defn, tail)
    }

    def subs(x: Term, y: Term) =
      DataCons(data.replace(x, y), defn, tail.subs(x, y))

    def caseFn(f: => Func[H, C])(arg: H): Option[C] =
      defn(data)(f)(arg) orElse (tail.caseFn(f)(arg))

    def rebuilt = DataCons(rebuild(data), defn, tail.rebuilt)

  }

}

import TermList.TermListOp

/**
  * indexed version of [[RecursiveDefinition]]
  */
abstract class IndexedRecursiveDefinition[H <: Term with Subs[H],
                                          F <: Term with Subs[F],
                                          C <: Term with Subs[C],
                                          Index <: HList: TermList,
                                          IF <: Term with Subs[IF],
                                          IDF <: Term with Subs[IDF],
                                          IDFT <: Term with Subs[IDFT]] {
  self =>
  val family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]

  val W: F

  val X: Typ[C]

  val defnData: Vector[Term]

  def caseFn(f: => IF)(arg: H): Option[C]

  case class Funcs(ind: Index) extends IndRecFunc[H, C, F] { fself =>
    val dom = family.pattern.typ(W, ind)

    val domW = self.W

    val index = ind.terms

    val codom = X

    val defnData = self.defnData

    val typ = dom ->: codom

    lazy val outer = self

    override lazy val hashCode = (outer, ind).hashCode

    override def equals(that: Any) = that match {
      case fn: IndexedRecursiveDefinition[a, b, c, d, e, f, g]#Funcs =>
        (outer, ind) == (fn.outer, fn.ind)
      case _ => false
    }

    def newobj = ??? // should not be called

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
                   Index <: HList: TermList,
                   IF <: Term with Subs[IF],
                   IDF <: Term with Subs[IDF],
                   IDFT <: Term with Subs[IDFT]](
      W: F,
      X: Typ[C],
      family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]
  ) extends IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT] {

    val defnData = Vector()

    def caseFn(f: => IF)(arg: H): Option[C] = None

    def subs(x: Term, y: Term) =
      Empty(W.replace(x, y), X.replace(x, y), family.subs(x, y))
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
      defn: D => IF => H => Option[C],
      tail: IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT]
  ) extends IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT] {
    val family = tail.family

    val defnData = data +: tail.defnData

    val W = tail.W

    val X = tail.X

    def caseFn(f: => IF)(arg: H): Option[C] =
      defn(data)(f)(arg) orElse (tail.caseFn(f)(arg))

    def subs(x: Term, y: Term) =
      DataCons(data.replace(x, y), defn, tail.subs(x, y))
  }
}
