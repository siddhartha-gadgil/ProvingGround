package provingground
import HoTT._
import Families._

import scala.language.implicitConversions
import scala.util._
import scala.language.existentials
import ConstructorPatterns.getArg

/**
 * @author gadgil
 */
class IndexedConstructorPatterns[F <: Term with Subs[F], 
  A <: Term with Subs[A], 
  I <: Term with Subs[I], C <: Term with Subs[C]](
      val typFmlyPtn: FmlyPtn[Term, C]{type FamilyType = F; type ArgType = A; type IterFunc = I}
      ) {outer =>
        type Cod = C
    sealed trait ConstructorPtn{self =>
    /**
     * Type of codomain X
     */
 //   type Cod <:  Term with Subs[Cod]
    
  //  val typFmlyPtn: FmlyPtn[Term, Cod]{type FamilyType = F; type ArgType = A; type IterFunc = I}
    
 //   type IterFunc = typFmlyPtn.IterFunc
    
    /**
     * argument for the final image
     */
    val arg : A
    
    type ConstructorType <: Term with Subs[ConstructorType]
    
    /**
     * type of a constructor for this pattern.
     */
    def apply(tps : F) : Typ[ConstructorType]
    
    /**
     * (scala) type of data for recursion corresponding to the single constructor
     */
    type RecDataType <: Term with Subs[RecDataType]

    /**
     * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
     */
    def recDom(w: F, x: Typ[Cod]) : Typ[RecDataType]
    
    /**
     * given a term, matches to see if this is the image of a given (quasi)-constructor.
     * returns simplification (wrapped in Some) if the term matches.
     * @param cons constructor, actually quasi-constructor, with which to match.
     * @param data definition data for the image of the constructor.
     * @param f the function being defined, to be applied recursively.
     */
    def recDef(cons: ConstructorType, data: RecDataType, f :  => I): Term => Option[Cod]
    
    def recModify(cons: ConstructorType)(data: RecDataType)(f : => I) : I = {
      def g = typFmlyPtn.fill(f)(arg)
      val fn = new Func[PairObj[A, Term], Cod]{
        lazy val W = g.dom
      
        lazy val dom = PairTyp(arg.typ.asInstanceOf[Typ[A]], W)
        
        lazy val codom = g.codom
      
        lazy val typ = dom ->: codom
      
        def newobj = this
      
        def act(a: PairObj[A, Term]) = (recDef(cons, data, f)(a)).getOrElse(g(a.second))
      
        def subs(x: Term, y: Term) = this
        }
      typFmlyPtn.curry(fn)
      }
    
    }
    
    case class iW(arg: A) extends ConstructorPtn{
      type ConstructorType = Term
      
  //    type Cod = C
      
      type RecDataType = Cod
      
      def apply(tps: F) = typFmlyPtn.contractType(tps)(arg)
      
      def recDom(w: F, x: Typ[Cod]) : Typ[RecDataType] = x
      
      def recDef(cons: ConstructorType, data: RecDataType, f :  => I): Term => Option[Cod] = {
        case (t: Term) if t == cons => Some(data)
        case _ => None
    }
    }
    
    /**
     * Eventually remove this, to avoid duplication of constructors
     */
 /*   case class SimpleFuncPtn[F <: Term with Subs[F], 
      A <: Term with Subs[A], 
      I <: Term with Subs[I],
      C<: Term with Subs[C]](
        tailArg: A, head : ConstructorPtn[F, A, I]{type Cod = C}) extends RecursiveConstructorPtn[F, A, I]{
      type Cod = C
      
      type ConstructorType = Func[Term, head.ConstructorType]
      
      val typFmlyPtn = head.typFmlyPtn
      
      val arg = head.arg
      
      type RecDataType = Func[Term, Func[Cod, head.RecDataType]]
      
      def apply(tps: F) = typFmlyPtn.contractType(tps)(tailArg) ->: head(tps)
      
      def recDom(w: F, x: Typ[Cod]) : Typ[RecDataType] =  apply(w) ->: x ->: head.recDom(w, x)
    }
   */ 
    
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
      type RecDataType = HeadRecDataType; type Cod = outer.Cod}

    /**
     * returns data for recursion to be passed on to the head given an argument (when matching with the construtor).
     */
    def headData(data: RecDataType, arg: ArgType, f : => I): HeadRecDataType

    def recDef(cons: ConstructorType, data: RecDataType, f :  => I): Term => Option[Cod] = {
      t =>
        for (arg <- getArg(cons)(t); term <-headfibre(arg).recDef(cons(arg), headData(data, arg, f), f)(t)) yield term
    }

  }
    
    
    
    case class FuncPtn(
        tail: FmlyPtn[Term, C], 
        tailArg: A, 
        head : ConstructorPtn{type Cod = C})extends RecursiveConstructorPtn{
      type ArgType = tail.Family

      type HeadType = head.ConstructorType
      
      type Cod = C
      
      type ConstructorType = Func[ArgType, head.ConstructorType]
      
      type HeadRecDataType = head.RecDataType
      
  //    val typFmlyPtn = head.typFmlyPtn
      
      val arg = head.arg
      
      val _head : ConstructorPtn{type ConstructorType = HeadType; type RecDataType = HeadRecDataType; type Cod = C} = head

      val headfibre = (t: ArgType) => _head
      
            
      type RecDataType = Func[tail.Family, Func[tail.TargetType, head.RecDataType]]
      
      def apply(tps: F) = {
        val w = typFmlyPtn.contractType(tps)(tailArg)
        FuncTyp[ArgType, head.ConstructorType](tail(w), head(tps))
      }
       
     
      
      def recDom(tps: F, x: Typ[Cod]) : Typ[RecDataType] =  {
        val w = typFmlyPtn.contractType(tps)(tailArg)
        tail(w) ->: tail.target(x) ->: head.recDom(tps, x)
      }
      
     def headData(data: RecDataType, arg: ArgType, f :  => I): HeadRecDataType = {
       val g = typFmlyPtn.fill(f)(tailArg)
        data(arg)(tail.induced(g)(arg))
      }
      
    }
    
    
    case class CnstFncPtn(
        tail: Typ[Term], 
        tailArg: A, 
        head : ConstructorPtn{type Cod = C})extends RecursiveConstructorPtn{
      type ArgType = Term

      type HeadType = head.ConstructorType
      
      type Cod = C
      
      type ConstructorType = Func[ArgType, head.ConstructorType]
      
      type HeadRecDataType = head.RecDataType
      
  //    val typFmlyPtn = head.typFmlyPtn
      
      val arg = head.arg
      
      val _head : ConstructorPtn{type ConstructorType = HeadType; type RecDataType = HeadRecDataType; type Cod = C} = head

      val headfibre = (t: ArgType) => _head
      
            
      type RecDataType = Func[Term, head.RecDataType]
      
      def apply(tps: F) = {
        FuncTyp[ArgType, head.ConstructorType](tail, head(tps))
      }
       
     
      
      def recDom(tps: F, x: Typ[Cod]) : Typ[RecDataType] =  {
        val w = typFmlyPtn.contractType(tps)(tailArg)
        tail ->: head.recDom(tps, x)
      }
      
     def headData(data: RecDataType, arg: ArgType, f :  => I): HeadRecDataType = {
       val g = typFmlyPtn.fill(f)(tailArg)
        data(arg)
      }
      
    }
    
    
    case class DepFuncPtn[U <: Term with Subs[U], V <: Term with Subs[V], W <: Term with Subs[W]](
        tail: FmlyPtn[Term, C], 
        tailArg: A, 
        arg: A,
        headfibre : Term => (ConstructorPtn{type ConstructorType = U; type RecDataType = V; type Cod = C})) extends RecursiveConstructorPtn{
      type ArgType = tail.Family

      type HeadType = U
      
      type Cod = C
      
      type ConstructorType = FuncLike[ArgType, U]
      
      type HeadRecDataType = V
      
  //    val typFmlyPtn = head.typFmlyPtn
      
//      val arg = head.arg

            
      type RecDataType = FuncLike[tail.Family, Func[tail.TargetType, V]]
      
      def apply(tps: F) = {
        val w = typFmlyPtn.contractType(tps)(tailArg)
        val a = tail(w).Var
        val fiber = lmbda(a)(headfibre(a)(tps))
        PiTyp[ArgType, U](fiber)
      }
       
     
      
      def recDom(tps: F, x: Typ[Cod]) : Typ[RecDataType] =  {
        val w = typFmlyPtn.contractType(tps)(tailArg)
        val a = tail(w).Var
        val fibre = lmbda(a)(tail.target(x) ->: headfibre(a).recDom(tps, x))
        PiTyp(fibre)
      }
      
     def headData(data: RecDataType, arg: ArgType, f :  => I): HeadRecDataType = {
       val g = typFmlyPtn.fill(f)(tailArg)
        data(arg)(tail.induced(g)(arg))
      }
      
    }
    
    
    case class CnstDepFuncPtn[U <: Term with Subs[U], V <: Term with Subs[V], W <: Term with Subs[W]](
        tail: Typ[Term], 
        arg: A,
        headfibre : Term => (ConstructorPtn{type ConstructorType = U; type RecDataType = V; type Cod = C})) extends RecursiveConstructorPtn{
      type ArgType = Term

      type HeadType = U
      
      type Cod = C
      
      type ConstructorType = FuncLike[ArgType, U]
      
      type HeadRecDataType = V
      
  //    val typFmlyPtn = head.typFmlyPtn
      
//      val arg = head.arg

            
      type RecDataType = FuncLike[Term, V]
      
      def apply(tps: F) = {
        val a = tail.Var
        val fiber = lmbda(a)(headfibre(a)(tps))
        PiTyp[ArgType, U](fiber)
      }
       
     
      
      def recDom(tps: F, x: Typ[Cod]) : Typ[RecDataType] =  {
        val a = tail.Var
        val fibre = lmbda(a)(headfibre(a).recDom(tps, x))
        PiTyp(fibre)
      }
      
     def headData(data: RecDataType, arg: ArgType, f :  => I): HeadRecDataType = {
        data(arg)
      }
      
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

  //  type Cod <: Term with Subs[Cod]
    /**
     * constructor-pattern for the constructor
     */
    val pattern : ConstructorPtn{type Cod = outer.Cod}

//    val typ: Typ[Term]

    /**
     * the constructor (function or constant) itself.
     */
    val cons: pattern.ConstructorType

    /**
     * the type for which this is a constructor
     */
    val W : F
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
  case class ConstructorDefn[U <: Term with Subs[U]](
      pattern: ConstructorPtn{type ConstructorType = U; type Cod = C},
      cons: U, W: F) extends Constructor{
    type ConstructorType = U

 //   type Cod = C
  }
  
  
   /**
   * rec(W)(X) is the value, defined recursively.
   * @tparam C codomain (scala) type
   * @tparam F full type of rec
   */
  trait RecFunction{self =>
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
    def pullback(X: Typ[C])(transform: I => I): FullType => FullType

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
      val caseFn : D => I => I = (d) => (f) => cons.pattern.recModify(cons.cons)(d)(f)
      RecFunctionCons[D](recdom, caseFn, this)
    }
  }

  def recFunction(conss: List[Constructor{type Cod = C}], W: Typ[Term], arg: A) = {
    val init : RecFunction = RecTail(W, arg)
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
  case class RecTail(W: Typ[Term], arg: A) extends RecFunction{
    type FullType = I

    private lazy val a = W.Var

    def recursion(X: Typ[C])(f: => FullType) = {
      val g = typFmlyPtn.fill(f)(arg)
      val fn = new Func[PairObj[A, Term], Cod]{
        
        lazy val dom = PairTyp(arg.typ.asInstanceOf[Typ[A]], W)
        
        lazy val codom = g.codom
      
        lazy val typ = dom ->: codom
      
        def newobj = this
      
        def act(argx: PairObj[A, Term]) = X.symbObj(ApplnSym(g, argx.second))
      
        def subs(x: Term, y: Term) = this                 
        }
      typFmlyPtn.curry(fn)
    }

    def pullback(X: Typ[C])(transform: I => I) = (g : I) => g
  }

  /**
   * cons for recursion function, i.e., adding a new constructor
   * @param dom domain
   * @param caseFn given (previous?) rec(W)(X) and function in domain (to be applied to value) matches pattern
   * @param tail previously added constructors
   */
  case class RecFunctionCons[D<: Term with Subs[D]](
      recdom: Typ[C] => Typ[D],
      caseFn : D => I => I,
      tail: RecFunction) extends RecFunction{
    val W = tail.W

  //  val X = tail.X

    type FullType = Func[D, tail.FullType]


    def pullback(X: Typ[C])(transform: I => I) = (g) =>
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