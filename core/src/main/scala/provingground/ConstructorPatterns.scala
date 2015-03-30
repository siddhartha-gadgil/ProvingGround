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
  sealed trait ConstructorPtn{self =>
    /**
     * Type of codomain X
     */
    type Cod <:  Term with Subs[Cod]

    /**
     * changes codomain and propagates changes to other types.
     */
    def withCod[CC <: Term with Subs[CC]] : ConstructorPtn{type ConstructorType = self.ConstructorType; type Cod = CC}

    /**
     * function pattern.
     */
    def -->:[V <: Term with Subs[V], T <: Term with Subs[T], D <: Term with Subs[D]](
        that : FmlyPtn[Term, Cod]) = FuncPtn[Cod](that, this)

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
    type ConstructorType <: Term with Subs[ConstructorType]

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
    def constructor(tp: => Typ[Term], name: AnySym) : Constructor = {
      val cons = apply(tp).symbObj(name)
      ConstructorDefn[ConstructorType, Cod](this, cons, tp)
    }



    /**
     * constructor for this pattern given inductive type, with a name symbol generated.
     */
    def newconstructor(tp: Typ[Term]): Constructor = {
      val cons = apply(tp).obj
      ConstructorDefn[ConstructorType, Cod](this, cons, tp)
    }

    val univLevel : Int

  }


  object ConstructorPtn{
    val W = IdW
  }




  /**
   * The constructor pattern W - the only valid head for constructor-patterns.
   */
  case object IdW extends ConstructorPtn{
    def apply(W : Typ[Term]) = W

    val univLevel = 0

    type ConstructorType = Term

    type RecDataType = Term

    def recDom(w: Typ[Term], x: Typ[Cod]) = x

    type Cod = Term

    def withCod[CC <: Term with Subs[CC]] = IdTarg[CC]

    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Term]): Term => Option[Term] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }

    case class IdTarg[C<: Term with Subs[C]]() extends ConstructorPtn{
      def apply(W : Typ[Term]) = W

    val univLevel = 0

    type ConstructorType = Term

    type RecDataType = C

    type Cod = C

    def recDom(w: Typ[Term], x: Typ[Cod]) = x

    def withCod[CC <: Term with Subs[CC]] = IdTarg[CC]

    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Cod]): Term => Option[Cod] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }
    }

  }



  /**
   * Functional extension of a type pattern
   */
  sealed trait RecursiveConstructorPtn extends ConstructorPtn{self =>
    /**
     * scala type of argument to constructor A -> ... (or A ~> ...)
     */
    type ArgType <: Term with Subs[ArgType]

    // type Cod = Term

    /**
     * scala type of the head T for constructor A -> T
     * for Pi-Types, the head may have varying HoTT type but must have fixed scala type.
     */
    type HeadType <: Term with Subs[HeadType]

    type ConstructorType <: FuncLike[ArgType, HeadType] with Subs[ConstructorType]

    /**
     * (scala) type of recursive data for head.
     */
    type HeadRecDataType <: Term

    /**
     * The head pattern, constant T for A -> T and T(a) for A ~> T(a)
     */
    val headfibre: ArgType => ConstructorPtn{type ConstructorType = HeadType;
      type RecDataType = HeadRecDataType; type Cod = self.Cod}

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
  case class FuncPtn[C <: Term with Subs[C]](tail: FmlyPtn[Term, C], head : ConstructorPtn{type Cod = C}) extends RecursiveConstructorPtn{self =>
    type ArgType = tail.Family

    type HeadType = head.ConstructorType

    type Cod = C

    def withCod[CC <: Term with Subs[CC]] = {
      val _res = FuncPtn[CC](tail.withCod[CC], head.withCod[CC])
      val res  = _res.asInstanceOf[FuncPtn[CC]{type ConstructorType = self.ConstructorType}]
      res
    }

    val _head : ConstructorPtn{type ConstructorType = HeadType; type RecDataType = HeadRecDataType; type Cod = C} = head

    val headfibre = (t: ArgType) => _head

    type ConstructorType = Func[ArgType, head.ConstructorType]

    type HeadRecDataType = head.RecDataType

    type RecDataType = Func[tail.Family, Func[tail.TargetType, head.RecDataType]]

    def recDom(w: Typ[Term], x: Typ[Cod]) = tail(w) ->: tail.target(x) ->: head.recDom(w, x)

    def headData(data: RecDataType, arg: ArgType, f :  => Func[Term, C]): HeadRecDataType = {
      data(arg)(tail.induced(f)(arg))
    }

    def apply(W : Typ[Term]) = FuncTyp[ArgType, head.ConstructorType](tail(W), head(W))

    val univLevel = max(head.univLevel, tail.univLevel)
  }

  /**
   * Extending a poly-pattern by a constant type, i.e., not depending on W.
   */
  case class CnstFncPtn(
      tail: Typ[Term],
      head : ConstructorPtn
      ) extends RecursiveConstructorPtn{self =>
    type ArgType = Term

    type HeadType = head.ConstructorType

    type Cod = head.Cod

    def withCod[CC <: Term with Subs[CC]] = {
      val _res = CnstFncPtn(tail, head.withCod[CC])
      val res  = _res.asInstanceOf[CnstFncPtn{type ConstructorType = self.ConstructorType; type Cod = CC}]
      res
    }

    val _head : ConstructorPtn{type ConstructorType = HeadType; type RecDataType = HeadRecDataType; type Cod = self.Cod} = head

    val headfibre = (t: ArgType) => _head


    type RecDataType = Func[Term, head.RecDataType]

    def recDom(w: Typ[Term], x: Typ[Cod]) = tail ->: head.recDom(w, x)

    type HeadRecDataType = head.RecDataType

    type ConstructorType = Func[Term, head.ConstructorType]

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
  case class DepFuncPtn[U <: Term with Subs[U], V <: Term with Subs[V], W <: Term with Subs[W], C <: Term with Subs[C]](tail: FmlyPtn[Term, C],
      headfibre : Term => (ConstructorPtn{type ConstructorType = U; type RecDataType = V; type Cod = C}),
      headlevel: Int = 0)/*(implicit su: ScalaUniv[U])*/ extends RecursiveConstructorPtn{self =>
    type ArgType = tail.Family

    type HeadType = U

    type ConstructorType = FuncLike[ArgType, U]

    type Cod = C

    def withCod[CC <: Term with Subs[CC]] = {

      val _res = DepFuncPtn(tail.withCod[CC], (t: Term) => headfibre(t).withCod[CC])
      val res  = _res.asInstanceOf[ConstructorPtn{type ConstructorType = self.ConstructorType; type Cod = CC}]
      res
    }

    type RecDataType = FuncLike[tail.Family, Func[tail.TargetType, V]]

    def recDom(w: Typ[Term], x: Typ[Cod]) = {
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
      headfibre : Term => (ConstructorPtn{type ConstructorType = U;
      type RecDataType = V; type Cod = C}), headlevel: Int = 0)/*(
      implicit su: ScalaUniv[U])*/ extends RecursiveConstructorPtn{self =>

    type ArgType = Term

    type HeadType = U

    type Cod = C

    def withCod[CC <: Term with Subs[CC]] = {

      val _res = CnstDepFuncPtn(tail, (t: Term) => headfibre(t).withCod[CC])
      val res  = _res.asInstanceOf[ConstructorPtn{type ConstructorType = self.ConstructorType; type Cod = CC}]
      res
    }

    type ConstructorType = FuncLike[Term, U]

    type RecDataType = FuncLike[Term, V]

    def recDom(w: Typ[Term], x: Typ[Cod]) = {
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
  trait Constructor{self =>
    /**
     * scala type, especially (nested) functions
     */
    type ConstructorType <: Term

    type Cod <: Term with Subs[Cod]
    /**
     * constructor-pattern for the constructor
     */
    val pattern : ConstructorPtn{type Cod = self.Cod}

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
      pattern: ConstructorPtn{type ConstructorType = U; type Cod = C},
      cons: U, W: Typ[Term]) extends Constructor{
    type ConstructorType = U

    type Cod = C
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
     def prepend(cons: Constructor{type Cod = C}) = {
      val recdom = (x: Typ[C]) => cons.pattern.recDom(cons.W, x)
      type D = cons.pattern.RecDataType
      val caseFn : D => Func[Term, C] => Func[Term, C] = (d) => (f) => cons.pattern.recModify(cons.cons)(d)(f)
      RecFunctionCons[D, C](recdom, caseFn, this)
    }
  }

  def recFunction[C <: Term with Subs[C]](conss: List[Constructor{type Cod = C}], W: Typ[Term]) = {
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
