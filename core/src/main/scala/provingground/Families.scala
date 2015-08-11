package provingground
import HoTT._
//import ScalaUniverses._
import math._
import scala.language.existentials

/**
 * @author gadgil
 */
object Families {

  /**
   * A pattern for families, e.g. of inductive types to be defined
   * for instance A -> B -> W, where W is the type to be defined;
   * ends with the type with members.
   * the pattern is a function of the type W.
   *
   */
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

    /**
      *  type of the total space, i.e., of W, sections to a universe
      **/
    type FamilyType <: Term with Subs[FamilyType]

    type Total <: Term with Subs[Total]

    /**
      *  Type of Curried function to X.
      **/
    type IterFunc <: Term with Subs[IterFunc]

    /**
      *  Type of curried function to typ[X]
      **/
    type IterTypFunc <: Term with Subs[IterTypFunc]

    type IterDepFunc <: Term with Subs[IterDepFunc]

    def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc]

    def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc): Typ[IterDepFunc]

    def domTotal(w: FamilyType) : Typ[Total]

//    def contract(f: Family)(arg: ArgType): O

    def fill(g: IterFunc)(arg: ArgType): Func[O, C]

    def depFill(g: IterDepFunc)(arg: ArgType) : FuncLike[O, C]

    def curry(f: Func[Total, Cod]): IterFunc

    def totalDomain(g: IterFunc) : Typ[Total]

    def uncurry(g: IterFunc) : Func[Total, Cod]

    def depCurry(f: FuncLike[Total, Cod]): IterDepFunc

    def depTotalDomain(g: IterDepFunc) : Typ[Total]

    def depUncurry(g: IterDepFunc) : FuncLike[Total, Cod] 

    def contractType(w: FamilyType)(arg: ArgType): Typ[O]

//    def collapse(mem: PairObj[Family, ArgType]) = contract(mem.first)(mem.second)

    /**
      *  type of total index
      **/
    type ArgType <: Term with Subs[ArgType]

    /**
     * scala type of target for induced functions, i.e., with O = Term
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

    def target(x: Typ[Cod]): Typ[TargetType]

    def depTarget(xs: Func[Term, Typ[Cod]]): Family => Typ[DepTargetType]


    def withCod[CC <: Term with Subs[CC]]: FmlyPtn[O, CC, Family]

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

  /**
   * The identity family
   */
  case class IdFmlyPtn[O <: Term with Subs[O], C <: Term with Subs[C]]() extends FmlyPtn[O, C, O] {
    def apply(W: Typ[O]) = W

    //    type Family =  O

    type FamilyType = Typ[O]

    type Total = O

    type IterFunc = Func[O, C]

    type IterTypFunc = Func[O, Typ[C]]

    type IterDepFunc = FuncLike[O, C]

    def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc] = w ->: x

    def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc) = PiTyp(xs)

    type ArgType = AtomicTerm

    def domTotal(w: FamilyType) : Typ[Total] = w

//    def contract(f: O)(arg: ArgType): O = f

//    def contractDep(f: O)(arg: ArgType) = f

    def fill(g: IterFunc)(arg: ArgType): Func[O, C] = g

    def depFill(g: IterDepFunc)(arg: ArgType): FuncLike[O, C] = g

    def curry(f: Func[Total, Cod]): IterFunc = f 

    def totalDomain(g: IterFunc) = g.dom

    def uncurry(g: IterFunc) : Func[Total, Cod] = g

    def depCurry(f: FuncLike[Total, Cod]): IterDepFunc = f

    def depTotalDomain(g: IterDepFunc) = g.dom

    def depUncurry(g: IterDepFunc) : FuncLike[Total, Cod] = g


    def contractType(w: FamilyType)(arg: ArgType): Typ[O] = w

    type TargetType = C

    type DepTargetType = C

    def target(x: Typ[Cod]) = x

    def depTarget(xs: Func[Term, Typ[Cod]]) = xs

    def withCod[CC <: Term with Subs[CC]] = IdFmlyPtn[O, CC]

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

  trait RecFmlyPtn[V <: Term with Subs[V], FV <: Term with Subs[FV], S <: Term with Subs[S], T <: Term, D <: Term with Subs[D], O <: Term with Subs[O], C <: Term with Subs[C]] extends FmlyPtn[O, C, FuncLike[Term, V]] {

    //    type Family <:  FuncLike[Term, V] with Subs[Family]

    type FamilyType <: FuncLike[Term, FV] with Subs[FamilyType]

    //   type ArgType <: AbsPair[Typ[Term], S] with Subs[AbsPair[Typ[Term], S]]

    //   def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

    type TargetType <: FuncLike[Term, T] with Subs[TargetType]

    type DepTargetType = FuncLike[Term, D]

    val tail: Typ[Term]

    val headfibre: Term => FmlyPtn[O, C, V] { type ArgType = S; type TargetType = T; type DepTargetType = D }

    def depTarget(xs: Func[Term, Typ[Cod]]) = (fmly: Family) =>
      {
        val a = tail.Var
        val b = fmly(a)
        val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
        PiTyp(targfibre)
    }

  }

  case class FuncFmlyPtn[V <: Term with Subs[V], FV <: Term with Subs[FV], I <: Term with Subs[I], IT <: Term with Subs[IT], 
DI <: Term with Subs[DI], S <: Term with Subs[S], T <: Term with Subs[T], D <: Term with Subs[D], O <: Term with Subs[O], C <: Term with Subs[C], HTot <: Term with Subs[HTot]](
    tail : Typ[Term],
    head : FmlyPtn[O, C, V] {
      type FamilyType = FV;
      type IterFunc = I;
      type IterTypFunc = IT;
      type IterDepFunc = DI;
      type ArgType = S;
      type TargetType = T; type DepTargetType = D;
      type Total = HTot;
    }
  )  extends FmlyPtn[O, C, Func[Term, V]] {
    def apply(W: Typ[O]) = FuncTyp[Term, V](tail, head(W))

    //  override type Family =  Func[Term, V]

    type FamilyType = Func[Term, FV]

    type Total = PairObj[Term, HTot]

    type IterFunc = Func[Term, head.IterFunc]

    type IterTypFunc = Func[Term, head.IterTypFunc]

    type IterDepFunc = FuncLike[Term, head.IterDepFunc]

    type DepTargetType = FuncLike[Term, D]

    def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc] = {
      val headtyp = head.iterFuncTyp(w(w.dom.obj), x)
      tail ->: headtyp
    }

    def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc): Typ[IterDepFunc] = {
      val a = tail.Var
      val headtyp =lmbda(a)(head.iterDepFuncTyp(w(w.dom.obj), xs(a)))
      PiTyp(headtyp)
    }


    type ArgType = PairObj[Term, S]

//    def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

    def fill(g: IterFunc)(arg: ArgType): Func[O, C] = {
      headfibre(arg.first).fill(g(arg.first))(arg.second)
    }

    def depFill(g: IterDepFunc)(arg: ArgType): FuncLike[O, C] = {
      headfibre(arg.first).depFill(g(arg.first))(arg.second)
    }


    def curry(f: Func[Total, Cod]): IterFunc = {
      val fdom = f.dom
      val z = fdom.Var
      val x = z.first
      val y = z.second
      lmbda(x)(
        headfibre(x).curry(
          lmbda(y)(f(z))
        )
      )
    }

    def domTotal(w: FamilyType) : Typ[Total] = PairTyp(w.dom, head.domTotal(w(w.dom.obj)))

    def totalDomain(g: IterFunc) = PairTyp(g.dom, head.totalDomain(g(g.dom.obj)))

    def uncurry(g: IterFunc) : Func[Total, Cod]= {
      val dom = totalDomain(g)
      val ab = dom.Var
      lmbda(ab)(head.uncurry(g(ab.first))(ab.second))
    }

    def depTotalDomain(g: IterDepFunc) =  PairTyp(g.dom, head.depTotalDomain(g(g.dom.obj)))

    def depUncurry(g: IterDepFunc) : FuncLike[Total, Cod]= {
      val dom = depTotalDomain(g)
      val ab = dom.Var
      lambda(ab)(head.depUncurry(g(ab.first))(ab.second))
    }

    def depCurry(f: FuncLike[Total, Cod]): IterDepFunc = {
      val fdom = f.dom
      val z = fdom.Var
      val x = z.first
      val y = z.second
      lambda(x)(
        headfibre(x).depCurry(
          lambda(y)(f(z))
        )
      )
    }



    def contractType(w: FamilyType)(arg: ArgType): Typ[O] = headfibre(arg).contractType(w(arg.first))(arg.second)

    type TargetType = Func[Term, T]

//    type DepTargetType = FuncLike[Term, D]

    def target(x: Typ[Cod]) = tail ->: head.target(x)

    def depTarget(xs: Func[Term, Typ[Cod]]) = (fmly: Family) =>
      {
        val a = tail.Var
        val b = fmly(a)
        val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
        PiTyp(targfibre)
    }


    //    type Cod = head.Cod

    def withCod[CC <: Term with Subs[CC]] = {
      val newHead = head.withCod[CC]
      FuncFmlyPtn[newHead.Family, newHead.FamilyType, newHead.IterFunc, newHead.IterTypFunc,  newHead.IterDepFunc, newHead.ArgType, newHead.TargetType, newHead.DepTargetType, O, CC, newHead.Total](tail, newHead)
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
  case class DepFuncFmlyPtn[V <: Term with Subs[V], FV <: Term with Subs[FV], I <: Term with Subs[I], IT<: Term with Subs[IT], DI <: Term with Subs[DI], S <: Term with Subs[S], T <: Term with Subs[T], D <: Term with Subs[D], O <: Term with Subs[O], C <: Term with Subs[C], HTot <: Term with Subs[HTot]](
    tail: Typ[Term],
    headfibre: Term => FmlyPtn[O, C, V] {
      type FamilyType = FV;
      type IterFunc = I;
      type IterTypFunc = IT;
      type IterDepFunc = DI;
      type ArgType = S;
      type TargetType = T; type DepTargetType = D;
      type Total = HTot
    },
    headlevel: Int = 0
  )  extends RecFmlyPtn[V, FV, S, T, D, O, C] {

    //    type Family =  FuncLike[Term, V]

    type FamilyType = FuncLike[Term, FV]

    type Total = DepPair[Term, HTot]

    type IterFunc = FuncLike[Term, I]

    type IterTypFunc = Func[Term, IT]

    type IterDepFunc = FuncLike[Term, DI]

    def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc] = {
      val a = tail.Var
      val fibre = lmbda(a)(headfibre(a).iterFuncTyp(w(a), x))
      PiTyp(fibre)
    }

    def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc): Typ[IterDepFunc] = {
      val a = tail.Var
      val fibre = lmbda(a)(headfibre(a).iterDepFuncTyp(w(a), xs(a)))
      PiTyp(fibre)
    }


    type ArgType = DepPair[Term, S]

//    def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

    def fill(g: IterFunc)(arg: ArgType): Func[O, C] = {
      headfibre(arg).fill(g(arg.first))(arg.second)
    }

    def depFill(g: IterDepFunc)(arg: ArgType): FuncLike[O, C] = {
      headfibre(arg).depFill(g(arg.first))(arg.second)
    }


    def curry(f: Func[Total, Cod]): IterFunc = {
      val fdom = f.dom
      val z = fdom.Var
      val x = z.first
      val y = z.second
      lmbda(x)(
        headfibre(x).curry(
          lmbda(y)(f(z))
        )
      )
    }

    def domTotal(w: FamilyType) : Typ[Total] = {
      val a = w.dom.Var
      val fibre = lmbda(a)(headfibre(a).domTotal(w(a)))
      SigmaTyp(fibre)
    }

    def totalDomain(g: IterFunc) = {
      val a = g.dom.Var
        val fibre = lmbda(a)(headfibre(a).totalDomain(g(a)))
      SigmaTyp(fibre)
    }


    def uncurry(g: IterFunc) : Func[Total, Cod]= {
      val dom = totalDomain(g)
      val ab = dom.Var
      lmbda(ab)(headfibre(ab.first).uncurry(g(ab.first))(ab.second))
    }


    def depCurry(f: FuncLike[Total, Cod]): IterDepFunc = {
      val fdom = f.dom
      val z = fdom.Var
      val x = z.first
      val y = z.second
      lambda(x)(
        headfibre(x).depCurry(
          lambda(y)(f(z))
        )
      )

    }

    def depTotalDomain(g: IterDepFunc) = {
      val a = g.dom.obj
        val fibre = lmbda(a)(headfibre(a).depTotalDomain(g(a)))
      SigmaTyp(fibre)
    }


    def depUncurry(g: IterDepFunc) : FuncLike[Total, Cod]= {
      val dom = depTotalDomain(g)
      val ab = dom.Var
      lambda(ab)(headfibre(ab.first).depUncurry(g(ab.first))(ab.second))
    }






    def contractType(w: FamilyType)(arg: ArgType): Typ[O] = headfibre(arg).contractType(w(arg.first))(arg.second)

    type TargetType = FuncLike[Term, T]

    // type DepTargetType = FuncLike[Term, D]

    def apply(W: Typ[O]) = {
      val x = tail.Var
      val fiber = lmbda(x)(headfibre(x)(W))
      //   val fiber = typFamily(tail,  (t : Term) => headfibre(t)(W))
      PiTyp[Term, V](fiber)
    }

    //  type Cod = C

    def target(x: Typ[Cod]) = {
      val a = tail.Var
      val targfibre = lmbda(a)(headfibre(a).target(x))
      PiTyp(targfibre)
    }

    def withCod[CC <: Term with Subs[CC]] = {
      val newHead = headfibre(tail.symbObj(Star))
      type VV = newHead.Family
      type FVV = newHead.FamilyType
      type SS = newHead.ArgType
      type TT = newHead.TargetType
      type DD = newHead.DepTargetType
      val newHeadFibre = (t: Term) =>
        (
          headfibre(t).withCod[CC].asInstanceOf[FmlyPtn[O, CC, VV] {
            type FamilyType = FVV;
            type IterFunc = I;
            type IterTypFunc = IT;
            type IterDepFunc = DI;
            type ArgType = SS;
            type TargetType = TT; type DepTargetType = DD;
            type Total = HTot
          }]
        )
      DepFuncFmlyPtn[VV, FVV, I, IT, DI, SS, TT, DD, O, CC, HTot](tail, newHeadFibre)
    }

    //    val head = headfibre(tail.symbObj(Star))

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
