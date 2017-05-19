package provingground
import HoTT._
//import ScalaUniv._
import math._
import scala.language.existentials

import scala.util.Try

/**
  * @author gadgil
  */
//object FamilyPattern {
/**
  * A pattern for families, e.g. of inductive types to be defined
  * for instance A -> B -> W, where W is the type to be defined;
  * ends with the type with members.
  *
  * given a codomain C, or a family of codomains, we can lift functions W -> C to functions on families.
  * @tparam O scala type of objects of W, i.e., members of the family.
  * @tparam F scala type of sections, e.g. A -> B -> W
  * @tparam C scala type of the codomain, needed to deduce types for induced functions.
  *
  * This is used in more than one way, which perhaps should be separated: for constructor types and for inductive families.
  * Hence we need two kinds of induced functions, e.g., (A -> B -> W) -> (A -> B -> X) and (A -> B -> W -> X).
  *
  * @param X codomain of the given function
  */
sealed trait FmlyPtn[
    O <: Term with Subs[O], C <: Term with Subs[C], F <: Term with Subs[F]] {
  self =>

  /**
    * the universe containing the type
    */
  val univLevel: Int

  type Cod = C

  /**
    * scala type (upper bound) for a member of the family, i.e., sections
    */
//   type Family = F

  /**
    *  type of  W, i.e., sections to a universe.
    */
  type FamilyType = F

  /**
    * type of the total space, i.e., all terms in some W.
    */
  type Total <: Term with Subs[Total]

  /**
    * project an element of the total type to its value, i.e., drop arguments
    */
  def value(x: Total): O

  /**
    *  Type of Curried function to X.
    */
  type IterFunc <: Term with Subs[IterFunc]

  /**
    *  Type of curried function to typ[X]
    */
  type IterTypFunc <: Term with Subs[IterTypFunc]

  type IterDepFunc <: Term with Subs[IterDepFunc]

  def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc]

  def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc): Typ[IterDepFunc]

  def domTotal(w: FamilyType): Typ[Total]

  //    def contract(f: Family)(arg: ArgType): O

  def fill(g: IterFunc)(arg: ArgType): Func[O, C]

  def depFill(g: IterDepFunc)(arg: ArgType): FuncLike[O, C]

  def curry(f: Func[Total, Cod]): IterFunc

  def curryTyp(w: Func[Total, Typ[Cod]]): IterTypFunc

  def totalDomain(g: IterFunc): Typ[Total]

  def totalTypDomain(g: IterTypFunc): Typ[Total]

  def uncurry(g: IterFunc): Func[Total, Cod]

  def uncurryTyp(g: IterTypFunc): Func[Total, Typ[Cod]]

  def depCurry(f: FuncLike[Total, Cod]): IterDepFunc

  def depTotalDomain(g: IterDepFunc): Typ[Total]

  def depUncurry(g: IterDepFunc): FuncLike[Total, Cod]

  def contractType(w: FamilyType)(arg: ArgType): Typ[O]

  //    def collapse(mem: PairTerm[Family, ArgType]) = contract(mem.first)(mem.second)

  /**
    *  type of total index
    */
  type ArgType <: Term with Subs[ArgType]

  def argOpt(l: List[Term]): Option[ArgType]

  def incl(term: O, arg: ArgType, w: FamilyType): Total

  //  def arg(x: Total): ArgType

  def withCod[CC <: Term with Subs[CC]](w: Typ[O]): FmlyPtn[O, CC, F]

  val me: FmlyPtn[O, C, F] {
    type IterFunc    = self.IterFunc;
    type IterTypFunc = self.IterTypFunc;
    type IterDepFunc = self.IterDepFunc;
    type ArgType     = self.ArgType;
    type Total       = self.Total
  } = self

  import FmlyPtn._

  def ->:[TT <: Term with Subs[TT]](
      tail: Typ[TT]): FmlyPtn[O, C, Func[TT, F]] = FuncFmlyPtn(tail, me)

  def ~>:[TT <: Term with Subs[TT]](
      tailVar: TT): FmlyPtn[O, C, FuncLike[TT, F]] = {
    val tail = tailVar.typ.asInstanceOf[Typ[TT]]
    val newHeadFibre = (t: TT) =>
      (
        self
          .subs(tailVar, t)
          .asInstanceOf[FmlyPtn[O, C, F] {
            //         type FamilyType = self.FamilyType;
            type IterFunc    = self.IterFunc;
            type IterTypFunc = self.IterTypFunc;
            type IterDepFunc = self.IterDepFunc;
            type ArgType     = self.ArgType;
            type Total       = self.Total
          }]
      )
    DepFuncFmlyPtn(tail, newHeadFibre)
  }

  def subs(x: Term, y: Term): FmlyPtn[O, C, F]
}

object FmlyPtn {
  def fmlyTyp[C <: Term with Subs[C], F <: Term with Subs[F]](
      ptn: FmlyPtn[Term, C, F]): Typ[F] = ptn match {
    case FuncFmlyPtn(tail: Typ[u], head: FmlyPtn[Term, _, w]) =>
      val headCast = head.asInstanceOf[FmlyPtn[Term, C, w]]
      val headTyp  = fmlyTyp[C, w](headCast)
      (tail ->: headTyp).asInstanceOf[Typ[F]]
    case DepFuncFmlyPtn(tail: Typ[u],
                        headfibre: (Function1[v, FmlyPtn[Term, _, w]]),
                        headlevel) =>
      val x        = tail.Var
      val headCast = headfibre(x).asInstanceOf[FmlyPtn[Term, C, w]]
      val headTyp  = fmlyTyp[C, w](headCast)
      (x ~>: headTyp).asInstanceOf[Typ[F]]
    case IdFmlyPtn() => Type.asInstanceOf[Typ[F]]
  }

  def fmly[C <: Term with Subs[C], F <: Term with Subs[F]](
      ptn: FmlyPtn[Term, C, F])(name: AnySym): F =
    fmlyTyp(ptn).symbObj(name)

  def getOpt[O <: Term with Subs[O], F <: Term with Subs[F]](typ: Typ[O],
                                                             fmlyTyp: Typ[F]) =
    Try(get[O, F](typ, fmlyTyp)).toOption

  def get[O <: Term with Subs[O], F <: Term with Subs[F]](
      typ: Typ[O],
      fmlyTyp: Typ[F]): FmlyPtn[O, Term, F] =
    fmlyTyp match {
      case `typ` => IdFmlyPtn[O, Term].asInstanceOf[FmlyPtn[O, Term, F]]
      case FuncTyp(dom: Typ[u], codom: Typ[v]) =>
        val head = get[O, v](typ, codom)
        val tail = dom
        val headCast: FmlyPtn[O, Term, head.FamilyType] {
//           type FamilyType = head.FamilyType;
          type IterFunc    = head.IterFunc;
          type IterTypFunc = head.IterTypFunc;
          type IterDepFunc = head.IterDepFunc;
          type ArgType     = head.ArgType;
          type Total       = head.Total
        }               = head
        val funcFmlyPtn = FuncFmlyPtn(tail, headCast)
        funcFmlyPtn.asInstanceOf[FmlyPtn[O, Term, F]]
      case tp: GenFuncTyp[u, v] =>
        val fibre     = tp.fib
        val tail      = tp.domain
        val a         = tail.Var
        val headfibre = (x: u) => get[O, v](typ, fibre(x))
        val newHead   = headfibre(tail.Var)
//         type VV = newHead.Family
        type I    = newHead.IterFunc
        type IT   = newHead.IterTypFunc
        type DI   = newHead.IterDepFunc
        type FVV  = newHead.FamilyType
        type SS   = newHead.ArgType
        type HTot = newHead.Total
        val newHeadFibre = (t: u) =>
          (
            headfibre(t).asInstanceOf[FmlyPtn[O, Term, FVV] {
//               type FamilyType = FVV;
              type IterFunc    = I;
              type IterTypFunc = IT;
              type IterDepFunc = DI;
              type ArgType     = SS;
              type Total       = HTot
            }]
        )
        DepFuncFmlyPtn(tail, newHeadFibre).asInstanceOf[FmlyPtn[O, Term, F]]
    }

/**
  * The identity family
  */
case class IdFmlyPtn[O <: Term with Subs[O], C <: Term with Subs[C]]()
    extends FmlyPtn[O, C, Typ[O]] {
  def apply(W: Typ[O]) = W

  def subs(x: Term, y: Term) = this
  //    type Family =  O

  // type FamilyType = Typ[O]

  type Total = O

  def value(x: Total): O = x

  type IterFunc = Func[O, C]

  type IterTypFunc = Func[O, Typ[C]]

  type IterDepFunc = FuncLike[O, C]

  def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc] = w ->: x

  def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc) = PiDefn(xs)

  def argOpt(l: List[Term]): Option[ArgType] =
    if (l.isEmpty) Some(Star) else None

  def incl(term: O, arg: ArgType, w: FamilyType): Total = term

  type ArgType = Term

  def arg(x: Total): Term = Star

  def domTotal(w: FamilyType): Typ[Total] = w

  //    def contract(f: O)(arg: ArgType): O = f

  //    def contractDep(f: O)(arg: ArgType) = f

  def fill(g: IterFunc)(arg: ArgType): Func[O, C] = g

  def depFill(g: IterDepFunc)(arg: ArgType): FuncLike[O, C] = g

  def curry(f: Func[Total, Cod]): IterFunc = f

  def curryTyp(w: Func[Total, Typ[Cod]]): IterTypFunc = w

  def totalDomain(g: IterFunc) = g.dom

  def totalTypDomain(g: IterTypFunc): Typ[Total] = g.dom

  def uncurry(g: IterFunc): Func[Total, Cod] = g

  def uncurryTyp(g: IterTypFunc): Func[Total, Typ[Cod]] = g

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

trait RecFmlyPtn[TT <: Term with Subs[TT],
                 FV <: Term with Subs[FV],
                 F <: FuncLike[TT, FV] with Subs[F],
                 S <: Term with Subs[S],
                 O <: Term with Subs[O],
                 C <: Term with Subs[C]]
    extends FmlyPtn[O, C, F] {

  //    type Family <:  FuncLike[Term, V] with Subs[Family]

//   type FamilyType <: FuncLike[TT, FV] with Subs[FamilyType]

  //   type ArgType <: AbsPair[Typ[Term], S] with Subs[AbsPair[Typ[Term], S]]

  //   def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

  val tail: Typ[TT]

  val headfibre: TT => FmlyPtn[O, C, FV] { type ArgType = S; }
}

case class FuncFmlyPtn[TT <: Term with Subs[TT],
                       FV <: Term with Subs[FV],
                       I <: Term with Subs[I],
                       IT <: Term with Subs[IT],
                       DI <: Term with Subs[DI],
                       S <: Term with Subs[S],
                       O <: Term with Subs[O],
                       C <: Term with Subs[C],
                       HTot <: Term with Subs[HTot]](
    tail: Typ[TT],
    head: FmlyPtn[O, C, FV] {
//     type FamilyType = FV;
      type IterFunc    = I;
      type IterTypFunc = IT;
      type IterDepFunc = DI;
      type ArgType     = S;
      type Total       = HTot;
    }
) extends FmlyPtn[O, C, Func[TT, FV]] {

  //  override type Family =  Func[Term, V]

//   type FamilyType = Func[TT, FV]

  type Total = AbsPair[TT, HTot]

  def value(x: Total): O = head.value(x.second)

  type IterFunc = FuncLike[TT, head.IterFunc]

  type IterTypFunc = FuncLike[TT, head.IterTypFunc]

  type IterDepFunc = FuncLike[TT, head.IterDepFunc]

  def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc] = {
    val headtyp = head.iterFuncTyp(w(w.dom.Var), x)
    tail ->: headtyp
  }

  def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc): Typ[IterDepFunc] = {
    val a       = tail.Var
    val headtyp = lmbda(a)(head.iterDepFuncTyp(w(w.dom.Var), xs(a)))
    PiDefn(headtyp)
  }

  type ArgType = AbsPair[TT, S]

  def argOpt(l: List[Term]): Option[ArgType] = l match {
    case x :: ys =>
      head.argOpt(ys) map ((t) => PairTerm(x.asInstanceOf[TT], t))
    case _ => None
  }

  def incl(term: O, arg: ArgType, w: FamilyType): Total =
    PairTerm(arg.first, head.incl(term, arg.second, w(arg.first)))

  //  def arg(x: Total) = PairTerm(x.first, head.arg(x.second))

  //    def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

  def fill(g: IterFunc)(arg: ArgType): Func[O, C] = {
    headfibre(arg.first).fill(g(arg.first))(arg.second)
  }

  def depFill(g: IterDepFunc)(arg: ArgType): FuncLike[O, C] = {
    headfibre(arg.first).depFill(g(arg.first))(arg.second)
  }

  def curry(f: Func[Total, Cod]): IterFunc = {
    val fdom = f.dom
    val z    = fdom.Var
    val x    = z.first
    val y    = z.second
    lambda(x)(
      headfibre(x).curry(
        lmbda(y)(f(z))
      )
    )
  }

  def curryTyp(w: Func[Total, Typ[Cod]]): IterTypFunc = {
    val wdom = w.dom
    val z    = wdom.Var
    val x    = z.first
    val y    = z.second
    lambda(x)(
      headfibre(x).curryTyp(
        lmbda(y)(w(z))
      )
    )
  }

  def domTotal(w: FamilyType): Typ[Total] = {
    val fstDom = w.dom
    val x      = fstDom.Var
    val fibre  = lmbda(x)(head.domTotal(w(x)))
    SigmaTyp(fibre)
  }
  def totalDomain(g: IterFunc) = {
    val fstDom = g.dom
    val x      = fstDom.Var
    val fibre  = lmbda(x)(head.totalDomain(g(x)))
    SigmaTyp(fibre)
  }

  def totalTypDomain(g: IterTypFunc): Typ[Total] = {
    val a     = g.dom.Var
    val fibre = lmbda(a)(headfibre(a).totalTypDomain(g(a)))
    SigmaTyp(fibre)
  }

  def uncurry(g: IterFunc): Func[Total, Cod] = {
    val dom       = totalDomain(g)
    val ab: Total = dom.Var
    lmbda(ab)(head.uncurry(g(ab.first))(ab.second))
  }

  def uncurryTyp(g: IterTypFunc): Func[Total, Typ[Cod]] = {
    val dom       = totalTypDomain(g)
    val ab: Total = dom.Var
    lmbda(ab)(head.uncurryTyp(g(ab.first))(ab.second))
  }

  def depTotalDomain(g: IterDepFunc) =
    ProdTyp(g.dom, head.depTotalDomain(g(g.dom.Var)))

  def depUncurry(g: IterDepFunc): FuncLike[Total, Cod] = {
    val dom       = depTotalDomain(g)
    val ab: Total = dom.Var
    lambda(ab)(head.depUncurry(g(ab.first))(ab.second))
  }

  def depCurry(f: FuncLike[Total, Cod]): IterDepFunc = {
    val fdom = f.dom
    val z    = fdom.Var
    val x    = z.first
    val y    = z.second
    lambda(x)(
      headfibre(x).depCurry(
        lambda(y)(f(z))
      )
    )
  }

  def contractType(w: FamilyType)(arg: ArgType): Typ[O] =
    headfibre(arg).contractType(w(arg.first))(arg.second)

  //    type Cod = head.Cod

  def withCod[CC <: Term with Subs[CC]](w: Typ[O]) = {
    val newHead = head.withCod[CC](w)
    FuncFmlyPtn[TT,
                newHead.FamilyType,
                newHead.IterFunc,
                newHead.IterTypFunc,
                newHead.IterDepFunc,
                newHead.ArgType,
                O,
                CC,
                newHead.Total](tail, newHead)
  }

  def subs(x: Term, y: Term) = {
    val newHead = head.subs(x, y)
    FuncFmlyPtn[TT,
                newHead.FamilyType,
                newHead.IterFunc,
                newHead.IterTypFunc,
                newHead.IterDepFunc,
                newHead.ArgType,
                O,
                C,
                newHead.Total](tail.replace(x, y), newHead)
  }

  val headfibre = (arg: Term) => head

  val univLevel = max(head.univLevel, univlevel(tail.typ))
}

/**
  * Extending by a constant type A a family of type patterns depending on (a : A).
  *
  */
case class DepFuncFmlyPtn[TT <: Term with Subs[TT],
                          FV <: Term with Subs[FV],
                          I <: Term with Subs[I],
                          IT <: Term with Subs[IT],
                          DI <: Term with Subs[DI],
                          S <: Term with Subs[S],
                          O <: Term with Subs[O],
                          C <: Term with Subs[C],
                          HTot <: Term with Subs[HTot]](
    tail: Typ[TT],
    headfibre: TT => FmlyPtn[O, C, FV] {
//     type FamilyType = FV;
      type IterFunc    = I;
      type IterTypFunc = IT;
      type IterDepFunc = DI;
      type ArgType     = S;
      type Total       = HTot
    },
    headlevel: Int = 0
) extends RecFmlyPtn[TT, FV, FuncLike[TT, FV], S, O, C] {

  //    type Family =  FuncLike[Term, V]

//   type FamilyType = FuncLike[TT, FV]

  type Total = AbsPair[TT, HTot]

  def value(x: Total) = headfibre(x.first).value(x.second)

  type IterFunc = FuncLike[TT, I]

  type IterTypFunc = Func[TT, IT]

  type IterDepFunc = FuncLike[TT, DI]

  def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc] = {
    val a     = tail.Var
    val fibre = lmbda(a)(headfibre(a).iterFuncTyp(w(a), x))
    piDefn(a)(headfibre(a).iterFuncTyp(w(a), x))
  }

  def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc): Typ[IterDepFunc] = {
    val a     = tail.Var
    val fibre = lmbda(a)(headfibre(a).iterDepFuncTyp(w(a), xs(a)))
    piDefn(a)(headfibre(a).iterDepFuncTyp(w(a), xs(a)))
  }

  type ArgType = AbsPair[TT, S]

  def argOpt(l: List[Term]): Option[ArgType] = l match {
    case x :: ys =>
      val xt = x.asInstanceOf[TT]
      headfibre(xt).argOpt(ys) map ((t) =>
                                      DepPair(
                                        xt,
                                        t,
                                        lmbda(xt)(t.typ.asInstanceOf[Typ[S]])))
    case _ => None
  }

  def incl(term: O, arg: ArgType, w: FamilyType): Total = {
    val x   = arg.first.typ.asInstanceOf[Typ[TT]].Var
    val fib = x :-> headfibre(x).domTotal(w(x))
    DepPair(arg.first,
            headfibre(arg.first).incl(term, arg.second, w(arg.first)),
            fib)
  }

  //    def contract(f: Family)(arg: ArgType): O = headfibre(arg).contract(f(arg.first))(arg.second)

  def fill(g: IterFunc)(arg: ArgType): Func[O, C] = {
    headfibre(arg.first).fill(g(arg.first))(arg.second)
  }

  def depFill(g: IterDepFunc)(arg: ArgType): FuncLike[O, C] = {
    headfibre(arg.first).depFill(g(arg.first))(arg.second)
  }

  def curry(f: Func[Total, Cod]): IterFunc = {
    val fdom = f.dom
    val z    = fdom.Var
    val x    = z.first
    val y    = z.second
    lmbda(x)(
      headfibre(x).curry(
        lmbda(y)(f(z))
      )
    )
  }

  def curryTyp(w: Func[Total, Typ[Cod]]): IterTypFunc = {
    val wdom = w.dom
    val z    = wdom.Var
    val x    = z.first
    val y    = z.second
    lmbda(x)(
      headfibre(x).curryTyp(
        lmbda(y)(w(z))
      )
    )
  }

  def domTotal(w: FamilyType): Typ[Total] = {
    val a     = w.dom.Var
    val fibre = lmbda(a)(headfibre(a).domTotal(w(a)))
    SigmaTyp(fibre)
  }

  def totalDomain(g: IterFunc) = {
    val a     = g.dom.Var
    val fibre = lmbda(a)(headfibre(a).totalDomain(g(a)))
    SigmaTyp(fibre)
  }

  def totalTypDomain(g: IterTypFunc): Typ[Total] = {
    val a     = g.dom.Var
    val fibre = lmbda(a)(headfibre(a).totalTypDomain(g(a)))
    SigmaTyp(fibre)
  }

  def uncurry(g: IterFunc): Func[Total, Cod] = {
    val dom       = totalDomain(g)
    val ab: Total = dom.Var
    lmbda(ab)(headfibre(ab.first).uncurry(g(ab.first))(ab.second))
  }

  def uncurryTyp(g: IterTypFunc): Func[Total, Typ[Cod]] = {
    val dom       = totalTypDomain(g)
    val ab: Total = dom.Var
    lmbda(ab)(headfibre(ab.first).uncurryTyp(g(ab.first))(ab.second))
  }

  def depCurry(f: FuncLike[Total, Cod]): IterDepFunc = {
    val fdom = f.dom
    val z    = fdom.Var
    val x    = z.first
    val y    = z.second
    lambda(x)(
      headfibre(x).depCurry(
        lambda(y)(f(z))
      )
    )
  }

  def depTotalDomain(g: IterDepFunc) = {
    val a     = g.dom.Var
    val fibre = lmbda(a)(headfibre(a).depTotalDomain(g(a)))
    SigmaTyp(fibre)
  }

  def depUncurry(g: IterDepFunc): FuncLike[Total, Cod] = {
    val dom       = depTotalDomain(g)
    val ab: Total = dom.Var
    lambda(ab)(headfibre(ab.first).depUncurry(g(ab.first))(ab.second))
  }

  def contractType(w: FamilyType)(arg: ArgType): Typ[O] =
    headfibre(arg.first).contractType(w(arg.first))(arg.second)

  // type DepTargetType = FuncLike[Term, D]

  //  type Cod = C

  def withCod[CC <: Term with Subs[CC]](w: Typ[O]) = {
    val newHead = headfibre(tail.Var)
//     type VV = newHead.Family
    type FVV = newHead.FamilyType
    type SS  = newHead.ArgType
    val newHeadFibre = (t: TT) =>
      (
        headfibre(t)
          .withCod[CC](w)
          .asInstanceOf[FmlyPtn[O, CC, FVV] {
//           type FamilyType = FVV;
            type IterFunc    = I;
            type IterTypFunc = IT;
            type IterDepFunc = DI;
            type ArgType     = SS;
            type Total       = HTot
          }]
      )
    DepFuncFmlyPtn[TT, FVV, I, IT, DI, SS, O, CC, HTot](tail, newHeadFibre)
  }

  def subs(x: Term, y: Term) = {
    val newHead = headfibre(tail.Var)
//     type VV = newHead.Family
    type FVV = newHead.FamilyType
    type SS  = newHead.ArgType
    val newHeadFibre = (t: TT) =>
      (
        headfibre(t)
          .subs(x, y)
          .asInstanceOf[FmlyPtn[O, C, FVV] {
//           type FamilyType = FVV;
            type IterFunc    = I;
            type IterTypFunc = IT;
            type IterDepFunc = DI;
            type ArgType     = SS;
            type Total       = HTot
          }]
      )
    DepFuncFmlyPtn[TT, FVV, I, IT, DI, SS, O, C, HTot](tail replace (x, y),
                                                       newHeadFibre)
  }

  //    val head = headfibre(tail.Var)

  //    type Family = FuncLike[Term, head.Family]

  val univLevel = max(univlevel(tail.typ), headlevel)
}
//}

}
