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
  sealed trait FmlyPtn[
    O <: Term with Subs[O], 
    C <: Term with Subs[C],
    F <:  Term with Subs[F]]{
    /**
     * the universe containing the type
     */
    val univLevel : Int

    /**
     * type of members
     */
    type MemberType = O

    /**
     * scala type (upper bound) for a member of the family.
     */
    type Family = F

    type FamilyType <: Term with Subs[FamilyType]

    type IterFunc <: Term with Subs[IterFunc]

    def iterFuncTyp(w: Typ[O], x : Typ[Cod]) : Typ[IterFunc]

    def contract(f: Family)(arg: ArgType): O

    def fill(g : IterFunc)(arg: ArgType) : Func[O, C]

    def curry(f : Func[PairObj[ArgType, O], Cod]) : IterFunc

    def contractType(w: FamilyType)(arg: ArgType) : Typ[O]

    def collapse(mem: PairObj[Family, ArgType]) = contract(mem.first)(mem.second)

    type ArgType <: Term with Subs[ArgType]

    /**
     * scala type of target for induced functions, i.e., with O = Term
     */
    type TargetType <: Term with Subs[TargetType]

    type DepTargetType <: Term with Subs[DepTargetType]

    /**
     * returns the type corresponding to the pattern, such as A -> W, given the (inductive) type W
     */
    def apply(tp : Typ[O]) : Typ[Family]

    def target(x : Typ[Cod]) : Typ[TargetType]

    type Cod = C

    def withCod[CC <: Term with Subs[CC]] : FmlyPtn[O, CC, Family]

    /**
    * function induced by f: W -> X of type (A -> W) -> (A -> X) etc
    *
    * @param f function from which to induce
    *
    * @param W the inductive type
    *
    * @param X codomain of the given function
    */
    def induced(f : Func[O, Cod]) : Family => TargetType

    /**
    * dependent function induced by dependent f: W -> X(s) of type (A -> W) -> ((a : A) ~> Xs(a)) etc
    *
    * @param f dependent function from which to induce
    *
    * @param W the inductive type
    *
    * @param Xs family of codomains of the given dependent function
    */
    def inducedDep(f : FuncLike[O, Cod]) : Family => DepTargetType
  }


    /**
   * The identity family
   */
  case class IdFmlyPtn[O <: Term with Subs[O], C <: Term with Subs[C]]() extends 
    FmlyPtn[O, C, O]{
    def apply(W : Typ[O]) = W

//    type Family =  O

    type FamilyType = Typ[O]

    type IterFunc = Func[O, C]

    def iterFuncTyp(w: Typ[O], x : Typ[Cod]) : Typ[IterFunc] = w ->: x

    type ArgType = AtomicTerm

    def contract(f: O)(arg: ArgType): O = f

    def fill(g : IterFunc)(arg: ArgType) : Func[O, C] = g

    def curry(f : Func[PairObj[ArgType, O], Cod]) : IterFunc = {
      val fdom = f.dom.asInstanceOf[PairTyp[ArgType, O]]
      val x = fdom.second.Var
      lmbda(x)(f(PairObj(Star, x)))
    }

    def contractType(w: FamilyType)(arg: ArgType) : Typ[O] = w

    type TargetType = C

    type DepTargetType = C

    def target(x: Typ[Cod]) = x

    def withCod[CC <: Term with Subs[CC]] = IdFmlyPtn[O, CC]

//    type Cod = C

//    type MemberType = O

    val univLevel = 0

    /**
     * induced function is the given one.
     */
    def induced(f : Func[O, C]) = f

    /**
     * induced function is the given one.
     */
    def inducedDep(f : FuncLike[O, C]) = f
  }

  trait RecFmlyPtn[V <: Term with Subs[V], FV <: Term with Subs[FV], S <: Term with Subs[S],
    T <: Term, D<: Term with Subs[D], O <: Term with Subs[O], C <: Term with Subs[C]] extends
        FmlyPtn[O, C, FuncLike[Term, V]]{

//    type Family <:  FuncLike[Term, V] with Subs[Family]

    type FamilyType <: FuncLike[Term, FV] with Subs[FamilyType]

 //   type ArgType <: AbsPair[Typ[Term], S] with Subs[AbsPair[Typ[Term], S]]

 //   def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

    type TargetType <: FuncLike[Term, T] with Subs[TargetType]

    type DepTargetType = FuncLike[Term, D]

    val tail : Typ[Term]

    val headfibre: Term => FmlyPtn[O, C, V]{type ArgType = S; type TargetType = T; type DepTargetType = D}
  }

  case class FuncFmlyPtn[V <: Term with Subs[V], FV <: Term with Subs[FV],
    I <: Term with Subs[I],
    S<: Term with Subs[S],
    T <: Term with Subs[T], D <: Term with Subs[D], O <: Term with Subs[O], C <: Term with Subs[C]](
      tail : Typ[Term],
      head : FmlyPtn[O, C, V]{type FamilyType = FV;
      type IterFunc = I;
      type ArgType = S;
      type TargetType = T; type DepTargetType = D})/*(
        implicit su: ScalaUniv[V])*/ extends FmlyPtn[O, C, Func[Term, V]]{
    def apply(W: Typ[O]) = FuncTyp[Term, V](tail, head(W))

 //  override type Family =  Func[Term, V]

    type FamilyType = Func[Term, FV]

    type IterFunc = Func[Term, head.IterFunc]
    
    type DepTargetType = FuncLike[Term, D]

    def iterFuncTyp(w: Typ[O], x : Typ[Cod]) : Typ[IterFunc] = {
      val headtyp = head.iterFuncTyp(w, x)
      tail ->: headtyp
    }

    type ArgType = PairObj[Term, S]

    def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

    def fill(g : IterFunc)(arg: ArgType) : Func[O, C] = {
      headfibre(arg.first).fill(g(arg.first))(arg.second)
    }

    def curry(f : Func[PairObj[ArgType, O], Cod]) : IterFunc = {
      val fdom = f.dom.asInstanceOf[PairTyp[ArgType, O]]
      val z = fdom.Var
      val x = z.first.first
      val y = z.first.second
      val yw = PairObj(y, z.second)
      lmbda(x)(
          headfibre(x).curry(
              lmbda(yw)(f(z))))
    }


    def contractType(w: FamilyType)(arg: ArgType) : Typ[O] = headfibre(arg).contractType(w(arg.first))(arg.second)

    type TargetType = Func[Term, T]

  //  type DepTargetType = FuncLike[Term, D]

    def target(x: Typ[Cod]) = tail ->: head.target(x)

//    type Cod = head.Cod

    def withCod[CC <: Term with Subs[CC]] ={
      val newHead = head.withCod[CC]
      FuncFmlyPtn[newHead.Family, newHead.FamilyType, newHead.IterFunc,
        newHead.ArgType, newHead.TargetType, newHead.DepTargetType, O, CC](tail, newHead)
    }

    val headfibre = (arg: Term) => head

    val univLevel = max(head.univLevel, univlevel(tail.typ))

    /**
     * inductively defining the induced function.
     * maps (g : tail --> head(W)) to func : tail --> head(X) given (head(W) --> head(X))
     *
     */
    def induced(f : Func[O, Cod]) : Family => TargetType = {
        val x = tail.Var
        val g = apply(f.dom).Var
        lmbda(g)(
            lmbda(x)(head.induced(f)(g(x))))

    }

    /**
     * inductively defining the induced function.
     * maps (g : tail --> head(W)) to func : (t : tail) ~> head(Xs(t)) given (head(W) --> (t: tail) ~> head(Xs(t)))
     *
     */
    def inducedDep(f : FuncLike[O, Cod]) : Family => DepTargetType = {
        val x = tail.Var
        val g = apply(f.dom).Var
        lambda(g)(
            lambda(x)(head.inducedDep(f)(g(x))))

    }
  }

    /**
   * Extending by a constant type A a family of type patterns depending on (a : A).
   *
   */
  case class DepFuncFmlyPtn[V <: Term with Subs[V],FV <: Term with Subs[FV],
    I <: Term with Subs[I],
    S<: Term with Subs[S],
    T <: Term with Subs[T], D <: Term with Subs[D], O<: Term with Subs[O], C<: Term with Subs[C]](
      tail: Typ[Term],
      headfibre : Term => FmlyPtn[O, C, V]{type FamilyType = FV;
      type IterFunc = I;
      type ArgType = S;
      type TargetType = T; type DepTargetType = D},
      headlevel: Int = 0)
      /*(implicit su: ScalaUniv[V])*/ extends RecFmlyPtn[V, FV, S, T, D, O, C]{

//    type Family =  FuncLike[Term, V]

    type FamilyType = FuncLike[Term, FV]

    type IterFunc = FuncLike[Term, I]

    def iterFuncTyp(w: Typ[O], x : Typ[Cod]) : Typ[IterFunc] = {
      val a = tail.Var
      val fibre = lmbda(a)(headfibre(a).iterFuncTyp(w, x))
      PiTyp(fibre)
    }

    type ArgType = DepPair[Term, S]

    def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

    def fill(g : IterFunc)(arg: ArgType) : Func[O, C] = {
      headfibre(arg).fill(g(arg.first))(arg.second)
    }

    def curry(f : Func[PairObj[ArgType, O], Cod]) : IterFunc = {
      val fdom = f.dom.asInstanceOf[PairTyp[ArgType, O]]
      val z = fdom.Var
      val x = z.first.first
      val y = z.first.second
      val yw = PairObj(y, z.second)
      lambda(x)(
          headfibre(x).curry(
              lmbda(yw)(f(z))))
    }

    def contractType(w: FamilyType)(arg: ArgType) : Typ[O] = headfibre(arg).contractType(w(arg.first))(arg.second)

    type TargetType = FuncLike[Term, T]

   // type DepTargetType = FuncLike[Term, D]

    def apply(W : Typ[O]) = {
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

    def withCod[CC <: Term with Subs[CC]] ={
      val newHead = headfibre(tail.symbObj(Star))
      type VV = newHead.Family
      type FVV = newHead.FamilyType
      type SS = newHead.ArgType
      type TT = newHead.TargetType
      type DD = newHead.DepTargetType
      val newHeadFibre = (t: Term) =>
        (
            headfibre(t).withCod[CC].asInstanceOf[FmlyPtn[O, CC, VV]{
              type FamilyType = FVV;
              type IterFunc = I;
              type ArgType = SS;
              type TargetType = TT; type DepTargetType = DD}]
            )
      DepFuncFmlyPtn[VV, FVV, I, SS, TT, DD, O, CC](tail, newHeadFibre)
    }

//    val head = headfibre(tail.symbObj(Star))

//    type Family = FuncLike[Term, head.Family]

     def induced(f : Func[O, Cod]) : Family => TargetType = {
        val x = tail.Var
        val g = apply(f.dom).Var
        lambda(g)(
            lambda(x)(headfibre(x).induced(f)(g(x))))

    }

    def inducedDep(f : FuncLike[O, Cod]) : Family => DepTargetType = {
        val x = tail.Var
        val g = apply(f.dom).Var
        lambda(g)(
            lambda(x)(headfibre(x).inducedDep(f)(g(x))))

    }

    val univLevel = max(univlevel(tail.typ), headlevel)
  }



}
