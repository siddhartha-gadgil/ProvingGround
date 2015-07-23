package provingground
import HoTT._
import Families._
import math._
//import ScalaUniverses._
import scala.util._
import scala.language.existentials

/**
 * constructors, their patterns, recursion
 * @author gadgil
 */
object ConstructorPatterns {
  /**
   * returns Some(x) if the term is f(x) with f given, otherwise None
   * @param func function f to match for f(x)
   */
    def getArg[D <: Term with Subs[D], U <: Term with Subs[U]](func : FuncLike[D, U]): Term => Option[D] = {
      case sym: Symbolic => sym.name match {
        case fx: ApplnSym[u, w] =>
          if (fx.func == func && fx.arg.typ == func.dom) Try(Some(fx.arg.asInstanceOf[D])).getOrElse(None)
            else getArg(func)(fx.func)
        case _ => None
      }
      case _ => None
    }

    /**
   * A composite pattern for inductive types.
   * Typically (A -> B -> W)-> C -> W -> (D -> W) -> W as a function of W
   * May have Pi-types instead of function types
   * Assumed to have fixed type for codomain  X.
   */
  sealed trait ConstructorPtn[
    Cod <: Term with Subs[Cod],
    CnstrctrType <: Term with Subs[CnstrctrType]]{self =>

    type ConstructorType = CnstrctrType
    /**
     * changes codomain and propagates changes to other types.
     */
    def withCod[CC <: Term with Subs[CC]] : ConstructorPtn[CC, ConstructorType]{type ConstructorType = self.ConstructorType;}

    /**
     * function pattern.
     */
    def -->:[V <: Term with Subs[V], 
      T <: Term with Subs[T], 
      D <: Term with Subs[D],
      F <: Term with Subs[F]](
        that : FmlyPtn[Term, Cod, F]) = FuncPtn(that, this)

  //  def -->:[UU >: U <: Term ](that : Typ[Term])(implicit self : Typ[Term]) : ConstructorPtn[FuncLike[Term, UU]] = {
  //    if (that == self) FuncPtn[UU](IdFmlyPtn[Term], this) else CnstFncPtn[UU](that, this)
  //  }

//    def :::[A](name : A)(implicit mytyp: Typ[Term]) : Constructor = constructor(mytyp, name)

    /**
     * returns HoTT type corresponding to the pattern given the (inductive) type W (to be the head).
     */
    def apply(tp : Typ[Term]) : Typ[ConstructorType]

    /**
     * (upper bound for) scala type of the constructor-pattern, especially to show constructor is a function.
     */
 //   type ConstructorType <: Term with Subs[ConstructorType]

    /**
     * (scala) type of data for recursion corresponding to the single constructor
     */
    type RecDataType <: Term with Subs[RecDataType]

    /**
     * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
     */
    def recDom(w: Typ[Term], x: Typ[Cod]) : Typ[RecDataType]

    /**
     * given a term, matches to see if this is the image of a given (quasi)-constructor.
     * returns simplification (wrapped in Some) if the term matches.
     * @param cons constructor, actually quasi-constructor, with which to match.
     * @param data definition data for the image of the constructor.
     * @param f the function being defined, to be applied recursively.
     */
    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Cod]): Term => Option[Cod]

    def recModify(cons: ConstructorType)(data: RecDataType)(f : => Func[Term, Cod]) : Func[Term, Cod] = new Func[Term, Cod]{
      lazy val dom = f.dom

      lazy val codom = f.codom

      lazy val typ = dom ->: codom

      def newobj = this

      def act(a: Term) = (recDef(cons, data, f)(a)).getOrElse(f(a))

      def subs(x: Term, y: Term) = this
    }

    /*
    def recModify(cons: ConstructorType)(data: RecDataType)(f : => Func[Term, Cod]) = {
      val a = f.dom.Var
      lmbda(a)((recDef(cons, data, f)(a)).getOrElse(f(a)))
    }
    */

    /**
     * invokes [[recDom]] after changing codomain type.
     */
    def rec[CC <: Term with Subs[CC]] = {
      val newPtn = withCod[CC]
      val fn : (newPtn.ConstructorType, newPtn.RecDataType, Func[Term, CC]) => Term => Option[CC] = {
        (cons, data, f) => (t) => newPtn.recDef(cons, data, f)(t)
      }
      fn
    }

    /**
     * constructor for this pattern given inductive type and name.
     */
    def constructor(tp: => Typ[Term], name: AnySym) : Constructor[Cod, ConstructorType] = {
      val cons = apply(tp).symbObj(name)
      ConstructorDefn[ConstructorType, Cod](this, cons, tp)
    }

    def cons(tp: => Typ[Term], name: AnySym)  = constructor(tp, name).cons

    /**
     * constructor for this pattern given inductive type, with a name symbol generated.
     */
    		def newconstructor(tp: Typ[Term]): Constructor[Cod, ConstructorType] = {
      val cons = apply(tp).obj
      ConstructorDefn[ConstructorType, Cod](this, cons, tp)
    }

    def cons(tp: => Typ[Term]) = newconstructor(tp).cons

    val univLevel : Int

  }


  object ConstructorPtn{
    val W = IdW
  }




  /**
   * The constructor pattern W - the only valid head for constructor-patterns.
   */
  case object IdW extends ConstructorPtn[Term, Term]{
    def apply(W : Typ[Term]) = W

    val univLevel = 0

//    type ConstructorType = Term

    type RecDataType = Term

    def recDom(w: Typ[Term], x: Typ[Term]) = x

//    type Cod = Term

    def withCod[CC <: Term with Subs[CC]] = IdTarg[CC]

    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Term]): Term => Option[Term] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }

    case class IdTarg[C<: Term with Subs[C]]() extends ConstructorPtn[C, Term]{
      def apply(W : Typ[Term]) = W

    val univLevel = 0

//    type ConstructorType = Term

    type RecDataType = C

//    type Cod = C

    def recDom(w: Typ[Term], x: Typ[C]) = x

    def withCod[CC <: Term with Subs[CC]] = IdTarg[CC]

    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, C]): Term => Option[C] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }
    }

  }



  /**
   * Functional extension of a type pattern
   */
  sealed trait RecursiveConstructorPtn[
    Cod <: Term with Subs[Cod],
    ArgT <: Term with Subs[ArgT],
    HeadT <: Term with Subs[HeadT],
    CT <: FuncLike[ArgT, HeadT] with Subs[CT]] extends ConstructorPtn[
      Cod, CT]{self =>
    /**
     * scala type of argument to constructor A -> ... (or A ~> ...)
     */
    type ArgType = ArgT

    // type Cod = Term

    /**
     * scala type of the head T for constructor A -> T
     * for Pi-Types, the head may have varying HoTT type but must have fixed scala type.
     */
    type HeadType =HeadT

 //   type ConstructorType <: FuncLike[ArgType, HeadType] with Subs[ConstructorType]

    /**
     * (scala) type of recursive data for head.
     */
    type HeadRecDataType <: Term

    /**
     * The head pattern, constant T for A -> T and T(a) for A ~> T(a)
     */
    val headfibre: ArgType => ConstructorPtn[Cod, HeadType]{
      type RecDataType = HeadRecDataType}

    /**
     * returns data for recursion to be passed on to the head given an argument (when matching with the construtor).
     */
    def headData(data: RecDataType, arg: ArgType, f : => Func[Term, Cod]): HeadRecDataType

    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Cod]): Term => Option[Cod] = {
      t =>
        for (arg <- getArg(cons)(t); term <-headfibre(arg).recDef(cons(arg), headData(data, arg, f), f)(t)) yield term
    }

  }

  /**
   * Extending a constructor-pattern by a type pattern.
   */
  case class FuncPtn[C <: Term with Subs[C], 
    F <: Term with Subs[F],
    HC <: Term with Subs[HC]](
    tail: FmlyPtn[Term, C, F], head : ConstructorPtn[C, HC]
    ) extends RecursiveConstructorPtn[C, F, HC, Func[F, HC]]{self =>
//    type ArgType = F

//    type HeadType = head.ConstructorType

//    type Cod = C

    def withCod[CC <: Term with Subs[CC]] = {
//      val _res = FuncPtn[CC](tail.withCod[CC], head.withCod[CC])
//      val res  = _res.asInstanceOf[FuncPtn[CC]{type ConstructorType = self.ConstructorType}]
//      res
  ???
    }

    val _head : ConstructorPtn[C, HeadType]{type RecDataType = HeadRecDataType} = head

    val headfibre = (t: ArgType) => _head

//    type ConstructorType = Func[ArgType, head.ConstructorType]

    type HeadRecDataType = head.RecDataType

    type RecDataType = Func[tail.Family, Func[tail.TargetType, head.RecDataType]]

    def recDom(w: Typ[Term], x: Typ[C]) = tail(w) ->: tail.target(x) ->: head.recDom(w, x)

    def headData(data: RecDataType, arg: ArgType, f :  => Func[Term, C]): HeadRecDataType = {
      data(arg)(tail.induced(f)(arg))
    }

    def apply(W : Typ[Term]) = FuncTyp[ArgType, head.ConstructorType](tail(W), head(W))

    val univLevel = max(head.univLevel, tail.univLevel)
  }

  /**
   * Extending a poly-pattern by a constant type, i.e., not depending on W.
   */
  case class CnstFncPtn[
    Cod <: Term with Subs[Cod],
    HC <: Term with Subs[HC]](
      tail: Typ[Term],
      head : ConstructorPtn[Cod, HC]
    ) extends RecursiveConstructorPtn[Cod, Term, HC, Func[Term, HC]]{self =>
 //   type ArgType = Term

 //   type HeadType = head.ConstructorType

  //  type Cod = head.Cod

    def withCod[CC <: Term with Subs[CC]] = {
      val _res = CnstFncPtn[CC, HC](tail, head.withCod[CC])
      val res  = _res.asInstanceOf[CnstFncPtn[CC, HC]]
      res
      ???
    }

    val _head : ConstructorPtn[Cod, HC]{
      type RecDataType = HeadRecDataType} = head

    val headfibre = (t: ArgType) => _head


    type RecDataType = Func[Term, head.RecDataType]

    def recDom(w: Typ[Term], x: Typ[Cod]) = tail ->: head.recDom(w, x)

    type HeadRecDataType = head.RecDataType

 //   type ConstructorType = Func[Term, head.ConstructorType]

    def headData(data: RecDataType, arg: ArgType, f :  => Func[Term, Cod]): HeadRecDataType = data(arg)

    def apply(W : Typ[Term]) = FuncTyp[Term, head.ConstructorType](tail, head(W))

    val univLevel = head.univLevel
  }

  /**
   * Extending a type pattern by a constant type to get (tail --> head).
   */
/**
   * Dependent extension of a poly-pattern by a type pattern.
   * XXX this may never be applicable
   */
  case class DepFuncPtn[
    U <: Term with Subs[U], 
    V <: Term with Subs[V], 
    W <: Term with Subs[W], 
    C <: Term with Subs[C],
    F <: Term with Subs[F]](tail: FmlyPtn[Term, C, F],
      headfibre : Term => (ConstructorPtn[C, U]{type RecDataType = V}),
      headlevel: Int = 0)/*(implicit su: ScalaUniv[U])*/ extends 
      RecursiveConstructorPtn[C, F, U, FuncLike[F, U]]{self =>
 //   type ArgType = F

 //   type HeadType = U

 //   type ConstructorType = FuncLike[ArgType, U]

  //  type Cod = C

    def withCod[CC <: Term with Subs[CC]] = {
//      val _res = DepFuncPtn[CC](tail.withCod[CC], (t: Term) => headfibre(t).withCod[CC])
//      val res  = _res.asInstanceOf[ConstructorPtn{type ConstructorType = self.ConstructorType}]
//      res
      ???
    }

    type RecDataType = FuncLike[tail.Family, Func[tail.TargetType, V]]

    def recDom(w: Typ[Term], x: Typ[C]) = {
      val a = tail(w).Var
      val fibre = lmbda(a)(tail.target(x) ->: headfibre(a).recDom(w, x))
      PiTyp(fibre)
    }

    type HeadRecDataType = V

    def headData(data: RecDataType, arg: ArgType, f :  => Func[Term, C]): HeadRecDataType = {
      val W = f.dom
      val X = f.codom
      val d = tail.induced(f)(arg)
      data(arg)(d)
    }

    def apply(W : Typ[Term]) : Typ[FuncLike[ArgType, U]]   = {
//      val head = headfibre(W.symbObj(Star))
//      val fiber = typFamily[Term, U](tail(W),  (t : Term) => headfibre(t)(W))
      val a = tail(W).Var
      val fiber = lmbda(a)(headfibre(a)(W))
      PiTyp[ArgType, U](fiber)
    }



//    type ConstructorType = Term

    val univLevel = max(tail.univLevel, headlevel)
  }

  /**
   * Dependent extension by a constant type  of a constructor-pattern depending on elements of that type.
   */
  case class CnstDepFuncPtn[U <: Term with Subs[U], V <: Term with Subs[V], C <: Term with Subs[C]](tail: Typ[Term],
      headfibre : Term => (ConstructorPtn[C, U]{
      type RecDataType = V}), headlevel: Int = 0)/*(
      implicit su: ScalaUniv[U])*/ extends 
      RecursiveConstructorPtn[C, Term, U, FuncLike[Term, U]]{self =>

//    type ArgType = Term

//    type HeadType = U

  //  type Cod = C

    def withCod[CC <: Term with Subs[CC]] = {

//      val _res = CnstDepFuncPtn[CC](tail, (t: Term) => headfibre(t).withCod[CC])
//      val res  = _res.asInstanceOf[ConstructorPtn{type ConstructorType = self.ConstructorType}]
//      res
  ???
    }

//    type ConstructorType = FuncLike[Term, U]

    type RecDataType = FuncLike[Term, V]

    def recDom(w: Typ[Term], x: Typ[C]) = {
      val a = tail.Var
      val fibre = lmbda(a)(headfibre(a).recDom(w, x))
      PiTyp(fibre)
    }

    type HeadRecDataType = V

    def headData(data: RecDataType, arg: ArgType, f :  => Func[Term, C]): HeadRecDataType = {
      data(arg)
    }

    def apply(W : Typ[Term]) : Typ[FuncLike[Term, U]] = {
 //     val fiber = typFamily[Term, U](tail,  (t : Term) => headfibre(t)(W))
      val a = W.Var
      val fiber = lmbda(a)(headfibre(a)(W))
      PiTyp[Term, U](fiber)
    }

//    type ConstructorType = Term

    val univLevel = headlevel
  }

    /**
   * Constructor for an inductive type, with given scala type and poly-pattern of this type.
   *
   * abstraction of ConstructorDefn mainly to allow different type parameters.
   */
  trait Constructor[Cod <: Term with Subs[Cod],
    CnstrType<: Term with Subs[CnstrType]]{self =>
    /**
     * scala type, especially (nested) functions
     */
    type ConstructorType <: Term

//    type Cod <: Term with Subs[Cod]
    /**
     * constructor-pattern for the constructor
     */
    val pattern : ConstructorPtn[Cod, CnstrType]

//    val typ: Typ[Term]

    /**
     * the constructor (function or constant) itself.
     */
    val cons: pattern.ConstructorType

    /**
     * the type for which this is a constructor
     */
    val W : Typ[Term]
  }



  /**
   * a constructor given by its parameters.
   *
   * @param pattern poly-pattern for the constructor.
   *
   * @param cons constructor function.
   *
   * @tparam U scala type of polypattern.
   */
  case class ConstructorDefn[U <: Term with Subs[U], C <: Term with Subs[C]](
      pattern: ConstructorPtn[C, U],
      cons: U, W: Typ[Term]) extends Constructor[C, U]{
    type ConstructorType = U

//    type Cod = C
  }


  /**
   * rec(W)(X) is the value, defined recursively.
   * @tparam C codomain (scala) type
   * @tparam F full type of rec
   */
  trait RecFunction[C<: Term with Subs[C]]{self =>
    /**
     * W in rec(W)(X)
     */
    val W: Typ[Term]

    /**
     * X in rec(W)(X)
     */
 //   val X : Typ[C]


    /**
     * (scala) type of rec(W)(X)
     */
    type FullType <: Term with Subs[FullType]

    /**
     * induced change to function of the type of rec(W)(X) given change on function W->X;
     * @param transform function W -> X by which we change functions W -> X, trying the case first.
     * @return induced changed function.
     */
    def pullback(X: Typ[C])(transform: Func[Term, C] => Func[Term, C]): FullType => FullType

    /**
     * given value for rec(W)(X) corresponding to earlier patterns, returns one including the new case.
     */
    def recursion(X: Typ[C])(f: => FullType): FullType

    /**
     * prepend a constructor
     */
     def prepend[U <: Term with Subs[U]](cons: Constructor[C, U]) = {
      val recdom = (x: Typ[C]) => cons.pattern.recDom(cons.W, x)
      type D = cons.pattern.RecDataType
      val caseFn : D => Func[Term, C] => Func[Term, C] = (d) => (f) => cons.pattern.recModify(cons.cons)(d)(f)
      RecFunctionCons[D, C](recdom, caseFn, this)
    }
  }

  def recFunction[C <: Term with Subs[C], U <: Term with Subs[U]](conss: List[Constructor[C, U]], W: Typ[Term]) = {
    val init : RecFunction[C] = RecTail[C](W)
    (init /: conss)(_ prepend _)
  }

/*
  case class RecProxy[C <: Term](W: Typ[Term], X : Typ[C]) extends AnySym{
    override def toString = s"rec($W)($X)"
  }
 */

  /**
   * container for rec(W)(X) in the case of no constructors.
   * rec(W)(X) is defined to be formal application of itself.
   * Lazy lambda used to avoid infinite loops.
   */
  case class RecTail[C <: Term with Subs[C]](W: Typ[Term]) extends RecFunction[C]{
    type FullType = Func[Term, C]

    private lazy val a = W.Var

    def recursion(X: Typ[C])(f: => FullType) = new LazyLambdaFixed(a, X.symbObj(ApplnSym(f, a)))

    def pullback(X: Typ[C])(transform: Func[Term, C] => Func[Term, C]) = (g : Func[Term, C]) => g
  }

  /**
   * cons for recursion function, i.e., adding a new constructor
   * @param dom domain
   * @param caseFn given (previous?) rec(W)(X) and function in domain (to be applied to value) matches pattern
   * @param tail previously added constructors
   */
  case class RecFunctionCons[D<: Term with Subs[D], C <: Term with Subs[C]](
      recdom: Typ[C] => Typ[D],
      caseFn : D => Func[Term, C] => Func[Term, C],
      tail: RecFunction[C]) extends RecFunction[C]{
    val W = tail.W

  //  val X = tail.X

    type FullType = Func[D, tail.FullType]


    def pullback(X: Typ[C])(transform: Func[Term, C] => Func[Term, C]) = (g) =>
      {
       val a = recdom(X).Var
      new LazyLambdaFixed(a, tail.pullback(X)(transform)(g(a)))
      }

    def recursion(X: Typ[C])(f: => FullType) ={
      val a = recdom(X).Var
      def fn(x: D) = tail.pullback(X)(caseFn(x))(tail.recursion(X)(f(x)))
      lmbda(a)(fn(a))
    }
  }


}
