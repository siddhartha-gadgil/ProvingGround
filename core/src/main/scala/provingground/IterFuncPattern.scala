package provingground

import HoTT._
import math._
import scala.language.existentials

import scala.util.Try

object IterFuncPattern{
  sealed trait FmlyPtn[O <: Term with Subs[O], C <: Term with Subs[C], F <: Term with Subs[F]] {
    /**
     * the universe containing the type
     */
    val univLevel: Int

    type Cod = C

    /**
     * scala type (upper bound) for a member of the family, i.e., sections
     */
    type Family = F


  //  def arg(x: Total): ArgType

    /**
     * scala type of target for induced functions
     */
    type TargetType <: Term with Subs[TargetType]

    /**
     * Note that the target may be a type family, not a type,
     *  so this is the actual type of objects.
     */
    type DepTargetType <: Term with Subs[DepTargetType]

    /**
     * returns the type corresponding to the pattern, such as A -> W, given the (inductive) type W,
     *  this is used mainly for constructor patterns, with the W being fixed, not for genuine families.
     */
    def apply(tp: Typ[O]): Typ[Family]

  /**
  * target scala type.
  */
    def target(x: Typ[Cod]): Typ[TargetType]

    /**
    * dependent target scala type.
    */
    def depTarget(xs: Func[O, Typ[Cod]]): Family => Typ[DepTargetType]

    def withCod[CC <: Term with Subs[CC]](w: Typ[O]): FmlyPtn[O, CC, Family]

    /**
     * function induced by f: W -> X of type (A -> W) -> (A -> X) etc
     *
     * @param f function from which to induce
     *
     * @param W the inductive type
     *
     * @param X codomain of the given function
     */
    def induced(f: Func[O, Cod]): Family => TargetType

    /**
     * dependent function induced by dependent f: W -> X(s) of type (A -> W) -> ((a : A) ~> Xs(a)) etc
     *
     * @param f dependent function from which to induce
     *
     * @param W the inductive type
     *
     * @param Xs family of codomains of the given dependent function
     */
    def inducedDep(f: FuncLike[O, Cod]): Family => DepTargetType
  }

  object FmlyPtn {
    def getOpt[O <: Term with Subs[O], F <: Term with Subs[F]](typ: Typ[O])(fmlyTyp: Typ[F]) =
      Try(get[O, Term, F](typ)(fmlyTyp)).toOption

    def get[O <: Term with Subs[O], C <: Term with Subs[C], F <: Term with Subs[F]](typ: Typ[O])(fmlyTyp: Typ[F]): FmlyPtn[O, C, F] =
      fmlyTyp match {
        case `typ` => IdFmlyPtn[O, C].asInstanceOf[FmlyPtn[O, C, F]]
        case FuncTyp(dom: Typ[u], codom: Typ[v]) =>
          val head = get[O, C, v](typ)(codom)
          val tail = dom
          val headCast: FmlyPtn[O, C, v] {
            type TargetType = head.TargetType;
            type DepTargetType = head.DepTargetType;
          } = head
          val funcFmlyPtn = FuncFmlyPtn(tail, headCast)
          funcFmlyPtn.asInstanceOf[FmlyPtn[O, C, F]]
        case tp: PiTyp[u, v] =>
          val fibre = tp.fibers
          val tail = fibre.dom
          val a = tail.Var
          val headfibre = (x: u) =>
            get[O, C, v](typ)(fibre(x))
          val newHead = headfibre(tail.Var)
          type VV = newHead.Family

          type TTT = newHead.TargetType
          type DD = newHead.DepTargetType
          val newHeadFibre = (t: u) =>
            (
              headfibre(t).asInstanceOf[FmlyPtn[O, C, VV] {

                type TargetType = TTT; type DepTargetType = DD;

              }]
            )
          DepFuncFmlyPtn(tail, newHeadFibre).asInstanceOf[FmlyPtn[O, C, F]]
      }
  }

  /**
   * The identity family
   */
  case class IdFmlyPtn[O <: Term with Subs[O], C <: Term with Subs[C]]() extends FmlyPtn[O, C, O] {
    def apply(W: Typ[O]) = W

    //    type Family =  O

    type FamilyType = Typ[O]

    type Total = O

    def value(x: Total): O = x

    type IterFunc = Func[O, C]

    type IterTypFunc = Func[O, Typ[C]]

    type IterDepFunc = FuncLike[O, C]

    def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc] = w ->: x

    def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc) = PiTyp(xs)

    type ArgType = AtomicTerm

    def arg(x: Total) : AtomicTerm = Star

    def domTotal(w: FamilyType): Typ[Total] = w

    //    def contract(f: O)(arg: ArgType): O = f

    //    def contractDep(f: O)(arg: ArgType) = f

    def fill(g: IterFunc)(arg: ArgType): Func[O, C] = g

    def depFill(g: IterDepFunc)(arg: ArgType): FuncLike[O, C] = g

    def curry(f: Func[Total, Cod]): IterFunc = f

    def totalDomain(g: IterFunc) = g.dom

    def uncurry(g: IterFunc): Func[Total, Cod] = g

    def depCurry(f: FuncLike[Total, Cod]): IterDepFunc = f

    def depTotalDomain(g: IterDepFunc) = g.dom

    def depUncurry(g: IterDepFunc): FuncLike[Total, Cod] = g

    def contractType(w: FamilyType)(arg: ArgType): Typ[O] = w

    type TargetType = C

    type DepTargetType = C

    def target(x: Typ[Cod]) = x

    def depTarget(xs: Func[O, Typ[Cod]]) = xs

    def withCod[CC <: Term with Subs[CC]](w: Typ[O]) = IdFmlyPtn[O, CC]

    //    type Cod = C

    //    type MemberType = O

    val univLevel = 0

    /**
     * induced function is the given one.
     */
    def induced(f: Func[O, C]) = f

    /**
     * induced function is the given one.
     */
    def inducedDep(f: FuncLike[O, C]) = f
  }

  trait RecFmlyPtn[TT <: Term with Subs[TT], V<: Term with Subs[V], T <: Term with Subs[T],
  D <: Term with Subs[D], O <: Term with Subs[O], C <: Term with Subs[C]] extends FmlyPtn[O, C, FuncLike[TT, V]] {

    //    type Family <:  FuncLike[Term, V] with Subs[Family]



    //   type ArgType <: AbsPair[Typ[Term], S] with Subs[AbsPair[Typ[Term], S]]

    //   def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

    type TargetType <: FuncLike[TT, T] with Subs[TargetType]

    type DepTargetType = FuncLike[TT, D]

    val tail: Typ[TT]

    val headfibre: TT => FmlyPtn[O, C, V] { type TargetType = T; type DepTargetType = D }

    def depTarget(xs: Func[O, Typ[Cod]]) = (fmly: Family) =>
      {
        val a = tail.Var
        val b = fmly(a)
        val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
        PiTyp(targfibre)
      }

  }

  case class FuncFmlyPtn[TT <: Term with Subs[TT], V <: Term with Subs[V],
   T <: Term with Subs[T],
  D <: Term with Subs[D], O <: Term with Subs[O],
  C <: Term with Subs[C]](
    tail: Typ[TT],
    head: FmlyPtn[O, C, V] {
      type TargetType = T; type DepTargetType = D;
    }
  ) extends FmlyPtn[O, C, Func[TT, V]] {
    def apply(W: Typ[O]) = FuncTyp[TT, V](tail, head(W))

    //  override type Family =  Func[Term, V]

    type DepTargetType = FuncLike[TT, D]


  //  def arg(x: Total) = PairObj(x.first, head.arg(x.second))

    //    def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

    type TargetType = Func[TT, T]

    //    type DepTargetType = FuncLike[Term, D]

    def target(x: Typ[Cod]) = tail ->: head.target(x)

    def depTarget(xs: Func[O, Typ[Cod]]) = (fmly: Family) =>
      {
        val a = tail.Var
        val b = fmly(a)
        val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
        PiTyp(targfibre)
      }

    //    type Cod = head.Cod

    def withCod[CC <: Term with Subs[CC]](w: Typ[O]) = {
      val newHead = head.withCod[CC](w)
      FuncFmlyPtn[TT, newHead.Family,  newHead.TargetType, newHead.DepTargetType, O, CC](tail, newHead)
    }

    val headfibre = (arg: Term) => head

    val univLevel = max(head.univLevel, univlevel(tail.typ))

    /**
     * inductively defining the induced function.
     * maps (g : tail --> head(W)) to func : tail --> head(X) given (head(W) --> head(X))
     *
     */
    def induced(f: Func[O, Cod]): Family => TargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lmbda(g)(
        lmbda(x)(head.induced(f)(g(x)))
      )

    }

    /**
     * inductively defining the induced function.
     * maps (g : tail --> head(W)) to func : (t : tail) ~> head(Xs(t)) given (head(W) --> (t: tail) ~> head(Xs(t)))
     *
     */
    def inducedDep(f: FuncLike[O, Cod]): Family => DepTargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(
        lambda(x)(head.inducedDep(f)(g(x)))
      )

    }
  }

  /**
   * Extending by a constant type A a family of type patterns depending on (a : A).
   *
   */
  case class DepFuncFmlyPtn[
    TT <: Term with Subs[TT],
    V <: Term with Subs[V],
    T <: Term with Subs[T], D <: Term with Subs[D], O <: Term with Subs[O],
    C <: Term with Subs[C]](
    tail: Typ[TT],
    headfibre: TT => FmlyPtn[O, C, V] {

      type TargetType = T; type DepTargetType = D;

    },
    headlevel: Int = 0
  ) extends RecFmlyPtn[TT, V,  T, D, O, C] {

    //    type Family =  FuncLike[Term, V]


    type TargetType = FuncLike[TT, T]

    // type DepTargetType = FuncLike[Term, D]

    def apply(W: Typ[O]) = {
      val x = tail.Var
      val fiber = lmbda(x)(headfibre(x)(W))
      //   val fiber = typFamily(tail,  (t : Term) => headfibre(t)(W))
      PiTyp[TT, V](fiber)
    }

    //  type Cod = C

    def target(x: Typ[Cod]) = {
      val a = tail.Var
      val targfibre = lmbda(a)(headfibre(a).target(x))
      PiTyp(targfibre)
    }

    def withCod[CC <: Term with Subs[CC]](w: Typ[O]) = {
      val newHead = headfibre(tail.Var)
      type VV = newHead.Family
      type TTT = newHead.TargetType
      type DD = newHead.DepTargetType
      val newHeadFibre = (t: TT) =>
        (
          headfibre(t).withCod[CC](w).asInstanceOf[FmlyPtn[O, CC, VV] {

            type TargetType = TTT; type DepTargetType = DD;

          }]
        )
      DepFuncFmlyPtn[TT, VV,  TTT, DD, O, CC](tail, newHeadFibre)
    }

    //    val head = headfibre(tail.Var)

    //    type Family = FuncLike[Term, head.Family]

    def induced(f: Func[O, Cod]): Family => TargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(
        lambda(x)(headfibre(x).induced(f)(g(x)))
      )

    }

    def inducedDep(f: FuncLike[O, Cod]): Family => DepTargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(
        lambda(x)(headfibre(x).inducedDep(f)(g(x)))
      )

    }

    val univLevel = max(univlevel(tail.typ), headlevel)
  }
}
