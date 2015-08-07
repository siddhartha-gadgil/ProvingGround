
package provingground
import HoTT._
import Families._

import scala.language.implicitConversions
import scala.util._
import scala.language.existentials
import ConstructorPattern._

/**
 * @author gadgil
 */
class IndexedConstructorPatterns[F <: Term with Subs[F], Ind <: Term with Subs[Ind], I <: Term with Subs[I], IT <: Term with Subs[IT], DI <: Term with Subs[DI], C <: Term with Subs[C], Fmly <: Term with Subs[Fmly]](
  val typFmlyPtn: FmlyPtn[Term, C, Fmly] { type FamilyType = F; type ArgType = Ind; type IterFunc = I; type IterDepFunc = DI }
) { outer =>
  type Cod = C
  sealed trait ConstructorPattern[Cnstr <: Term with Subs[Cnstr]] { self =>
    /**
     * Type of codomain X
     */
    //   type Cod <:  Term with Subs[Cod]

    //  val typFmlyPtn: FmlyPtn[Term, Cod]{type FamilyType = F; type ArgType = A; type IterFunc = I}

    //   type IterFunc = typFmlyPtn.IterFunc

    /**
     * argument for the final image
     */
    val index: Ind

    /**
     * type of a constructor for this pattern.
     */
    type ConstructorType = Cnstr

    def apply(tps: F): Typ[ConstructorType]

    /**
     * (scala) type of data for recursion corresponding to the single constructor
     */
    type RecDataType <: Term with Subs[RecDataType]
 
    type InducDataType <: Term with Subs[InducDataType]

    /**
     * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
     */
    def recDom(w: F, x: Typ[Cod]): Typ[RecDataType]

    def inducDom(w: F, xs: Func[Term, Typ[Cod]])(cons: ConstructorType) : Typ[InducDataType]
    /**
     * given a term, matches to see if this is the image of a given (quasi)-constructor.
     * returns simplification (wrapped in Some) if the term matches.
     * @param cons constructor, actually quasi-constructor, with which to match.
     * @param data definition data for the image of the constructor.
     * @param f the function being defined, to be applied recursively.
     */
    def recDef(cons: ConstructorType, data: RecDataType, f: => I): Term => Option[Cod]

    def inducDef(cons: ConstructorType, data : InducDataType, f : => DI): Term => Option[Cod] 

    def recModify(cons: ConstructorType)(data: RecDataType)(f: => I)(g: => I): I = {
      lazy val ff = typFmlyPtn.fill(f)(index)
      lazy val gg = typFmlyPtn.fill(g)(index)
      val fn = new Func[PairObj[Ind, Term], Cod] {
        lazy val W = ff.dom

        lazy val dom = PairTyp(index.typ.asInstanceOf[Typ[Ind]], W)

        lazy val codom = ff.codom

        lazy val typ = dom ->: codom

        def newobj = this

        def act(a: PairObj[Ind, Term]) = (recDef(cons, data, f)(a)).getOrElse(gg(a.second))

        def subs(x: Term, y: Term) = this
      }
      typFmlyPtn.curry(fn)
    }

  def inducModify(cons: ConstructorType)(data: InducDataType)(
    f: => DI
  )(g: => DI): DI = {
      lazy val ff = typFmlyPtn.depFill(f)(index)
      lazy val gg = typFmlyPtn.depFill(g)(index)

    val fn = new FuncLike[PairObj[Ind, Term], Cod] {
    lazy val W = ff.dom

    lazy val dom = PairTyp(index.typ.asInstanceOf[Typ[Ind]], W)

    lazy val a = "a" :: dom

    lazy val depcodom = ff.depcodom

    lazy val fibre = lmbda(a)(depcodom(a))

    lazy val typ = PiTyp(fibre)

    def newobj = this

    def act(a: PairObj[Ind, Term]) = (inducDef(cons, data, f)(a)).getOrElse(gg(a.second))

    def subs(x: Term, y: Term) = this

    override def toString = f.toString
    }
    typFmlyPtn.depCurry(fn)

  }
  }


  




  case class iW(index: Ind) extends ConstructorPattern[Term] {
 //   type ConstructorType = Term

    //    type Cod = C

    type RecDataType = Cod

    type InducDataType = Cod

    def apply(tps: F) = typFmlyPtn.contractType(tps)(index)

    def recDom(w: F, x: Typ[Cod]): Typ[RecDataType] = x

    def inducDom(w: F, xs: Func[Term, Typ[Cod]])(cons: Term) = xs(cons)

    def recDef(cons: ConstructorType, data: RecDataType, f: => I): Term => Option[Cod] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }

    def inducDef(cons: ConstructorType, data : InducDataType, f : => DI): Term => Option[Cod] =  {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }
  }


  /**
   * Functional extension of a type pattern
   */
  sealed trait RecursiveConstructorPattern[ArgT <: Term with Subs[ArgT], HeadT <: Term with Subs[HeadT], CT <: FuncLike[ArgT, HeadT] with Subs[CT]] extends ConstructorPattern[CT] { self =>
    /**
     * scala type of argument to constructor A -> ... (or A ~> ...)
     */
    type ArgType = ArgT

    // type Cod = Term

    /**
     * scala type of the head T for constructor A -> T
     * for Pi-Types, the head may have varying HoTT type but must have fixed scala type.
     */
    type HeadType = HeadT

  //  type ConstructorType =CT

    /**
     * (scala) type of recursive data for head.
     */
    type HeadRecDataType <: Term

    type HeadInducDataType <: Term

    /**
     * The head pattern, constant T for A -> T and T(a) for A ~> T(a)
     */
    val headfibre: ArgType => ConstructorPattern[HeadType] {
      type RecDataType = HeadRecDataType;
      type InducDataType = HeadInducDataType
    }

    /**
     * returns data for recursion to be passed on to the head given an argument (when matching with the construtor).
     */
    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType

    def recDef(cons: ConstructorType, data: RecDataType, f: => I): Term => Option[Cod] = {
      t =>
        for (arg <- getArg(cons)(t); term <- headfibre(arg).recDef(cons(arg), headData(data, arg, f), f)(t)) yield term
    }

    def headInducData(data: InducDataType, arg: ArgType, f : => DI) : HeadInducDataType


    def inducDef(cons: ConstructorType, data : InducDataType, f : => DI): Term => Option[Cod] = {
      t =>
        for (arg <- getArg(cons)(t); term <- headfibre(arg).inducDef(cons(arg), headInducData(data, arg, f), f)(t)) yield term
    }
  }

  case class FuncPtn[TF <: Term with Subs[TF], HC <: Term with Subs[HC]](
    tail: FmlyPtn[Term, C, TF],
    tailIndex: Ind,
    head: ConstructorPattern[HC]
  ) extends RecursiveConstructorPattern[TF, HC, Func[TF, HC]] {
//    type ArgType = tail.Family

//    type HeadType = head.ConstructorType

    type Cod = C

//    type ConstructorType = Func[ArgType, head.ConstructorType]

    type HeadRecDataType = head.RecDataType

    type HeadInducDataType = head.InducDataType

    //    val typFmlyPtn = head.typFmlyPtn

    val index = head.index

    val _head: ConstructorPattern[HeadType] {type RecDataType = HeadRecDataType; type InducDataType = HeadInducDataType} = head

    val headfibre = (t: ArgType) => _head

    type RecDataType = Func[tail.Family, Func[tail.TargetType, head.RecDataType]]

    type InducDataType = FuncLike[tail.Family, Func[tail.DepTargetType, head.InducDataType]]

    def apply(tps: F) = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      FuncTyp[ArgType, head.ConstructorType](tail(w), head(tps))
    }

    def recDom(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      tail(w) ->: tail.target(x) ->: head.recDom(tps, x)
    }

    def inducDom(tps: F, xs: Func[Term, Typ[C]])(cons: ConstructorType) : Typ[InducDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
    val a = tail(w).Var
    val headcons = cons(a)
    val fibre = lmbda(a)(tail.depTarget(xs)(a) ->: head.inducDom(tps, xs)(headcons))
    PiTyp(fibre)
  }


    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      val g = typFmlyPtn.fill(f)(tailIndex)
      data(arg)(tail.induced(g)(arg))
    }

    def headInducData(data: InducDataType, arg: ArgType, f : => DI) : HeadInducDataType = {
      val g= typFmlyPtn.depFill(f)(tailIndex)
      data(arg)(tail.inducedDep(g)(arg))
    }

  }

  case class CnstFncPtn[HC <: Term with Subs[HC]](
    tail: Typ[Term],
    tailIndex: Ind,
    head: ConstructorPattern[HC]
  ) extends RecursiveConstructorPattern[Term, HC, Func[Term, HC]] {
//    type ArgType = Term

//    type HeadType = head.ConstructorType

//    type Cod = C

//    type ConstructorType = Func[ArgType, head.ConstructorType]

    type HeadRecDataType = head.RecDataType

    type HeadInducDataType = head.InducDataType

    //    val typFmlyPtn = head.typFmlyPtn

    val index = head.index

    val _head: ConstructorPattern[HC] { type ConstructorType = HeadType; type RecDataType = HeadRecDataType; 
      type InducDataType = HeadInducDataType} = head

    val headfibre = (t: ArgType) => _head

    type RecDataType = Func[Term, head.RecDataType]

    type InducDataType = FuncLike[Term, head.InducDataType]

    def apply(tps: F) = {
      FuncTyp[ArgType, head.ConstructorType](tail, head(tps))
    }

    def recDom(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
//      val w = typFmlyPtn.contractType(tps)(tailIndex)
      tail ->: head.recDom(tps, x)
    }

  def inducDom(tps: F, xs: Func[Term, Typ[Cod]])(cons: ConstructorType) : Typ[InducDataType] = {
    val a = tail.Var
    val headcons = cons(a)
    val fibre = lmbda(a)(head.inducDom(tps, xs)(headcons))
    PiTyp(fibre)
  }


    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      val g = typFmlyPtn.fill(f)(tailIndex)
      data(arg)
    }

    def headInducData(data: InducDataType, arg: ArgType, f : => DI) : HeadInducDataType = {
      val g= typFmlyPtn.depFill(f)(tailIndex)
      data(arg)
    }


  }

  case class DepFuncPtn[U <: Term with Subs[U], V <: Term with Subs[V], VV <: Term with Subs[VV], W <: Term with Subs[W], TF <: Term with Subs[TF]](
    tail: FmlyPtn[Term, C, TF],
    tailIndex: Ind,
    index: Ind,
    headfibre: Term => (ConstructorPattern[U] {type RecDataType = V; type InducDataType = VV})
  ) extends RecursiveConstructorPattern[TF, U, FuncLike[TF, U]] {
//    type ArgType = tail.Family

//    type HeadType = U

    type Cod = C

//    type ConstructorType = FuncLike[ArgType, U]

    type HeadRecDataType = V

    type HeadInducDataType = VV

    //    val typFmlyPtn = head.typFmlyPtn

    //      val arg = head.arg

    type RecDataType = FuncLike[tail.Family, Func[tail.TargetType, V]]

    type InducDataType = FuncLike[tail.Family, Func[tail.DepTargetType, VV]]

    def apply(tps: F) = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      val a = tail(w).Var
      val fiber = lmbda(a)(headfibre(a)(tps))
      PiTyp[ArgType, U](fiber)
    }

    def recDom(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      val a = tail(w).Var
      val fibre = lmbda(a)(tail.target(x) ->: headfibre(a).recDom(tps, x))
      PiTyp(fibre)
    }

  def inducDom(tps: F, xs: Func[Term, Typ[Cod]])(cons: ConstructorType) : Typ[InducDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
    val a = tail(w).Var
    val headcons = cons(a)
    val fibre = lmbda(a)(tail.depTarget(xs)(a) ->: headfibre(a).inducDom(tps, xs)(headcons))
    PiTyp(fibre)
  }

    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      val g = typFmlyPtn.fill(f)(tailIndex)
      data(arg)(tail.induced(g)(arg))
    }

    def headInducData(data: InducDataType, arg: ArgType, f : => DI) : HeadInducDataType = {
      val g= typFmlyPtn.depFill(f)(tailIndex)
      data(arg)(tail.inducedDep(g)(arg))
    }

  }

  case class CnstDepFuncPtn[U <: Term with Subs[U], V <: Term with Subs[V], VV<: Term with Subs[VV], W <: Term with Subs[W]](
    tail: Typ[Term],
    index : Ind,
    headfibre: Term => (ConstructorPattern[U] {type RecDataType = V; type InducDataType = VV})
  ) extends RecursiveConstructorPattern[Term, U, FuncLike[Term, U]] {
//    type ArgType = Term

//    type HeadType = U

//    type Cod = C

//    type ConstructorType = FuncLike[ArgType, U]

    type HeadRecDataType = V

    type HeadInducDataType = VV

    //    val typFmlyPtn = head.typFmlyPtn

    //      val arg = head.arg

    type RecDataType = FuncLike[Term, V]

    type InducDataType = FuncLike[Term, VV]

    def apply(tps: F) = {
      val a = tail.Var
      val fiber = lmbda(a)(headfibre(a)(tps))
      PiTyp[ArgType, U](fiber)
    }

    def recDom(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      val a = tail.Var
      val fibre = lmbda(a)(headfibre(a).recDom(tps, x))
      PiTyp(fibre)
    }

  def inducDom(tps: F, xs: Func[Term, Typ[C]])(cons: ConstructorType) : Typ[InducDataType] = {
    val a = tail.Var
    val headcons = cons(a)
    val fibre = lmbda(a)(headfibre(a).inducDom(tps, xs)(headcons))
    PiTyp(fibre)
  }



    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      data(arg)
    }

    def headInducData(data: InducDataType, arg: ArgType, f : => DI) : HeadInducDataType = {
      data(arg)
    }


  }

  /**
   * Constructor for an inductive type, with given scala type and poly-pattern of this type.
   *
   * abstraction of ConstructorDefn mainly to allow different type parameters.
   */
  trait Constructor[Cnstr<: Term with Subs[Cnstr]] { self =>
    /**
     * scala type, especially (nested) functions
     */
    type ConstructorType = Cnstr 

    //  type Cod <: Term with Subs[Cod]
    /**
     * constructor-pattern for the constructor
     */
    val pattern: ConstructorPattern[Cnstr]

    //    val typ: Typ[Term]

    /**
     * the constructor (function or constant) itself.
     */
    val cons: pattern.ConstructorType

    /**
     * the type for which this is a constructor
     */
    val W: F
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
    pattern: ConstructorPattern[U],
    cons: U, W: F
  ) extends Constructor[U]




// Pasted from RecursiveDefinition

trait IndexedRecursiveDefinition {self =>
    /**
   * W in rec(W)(X)
   */
  val W: F

  /**
   * X in rec(W)(X)
   */
   val X : Typ[C]
  
  /**
   * recursive definition, with offspring applying f.
   */
  def recursion(f : => I) : I
  
  /**
   * the function to use, applying itself to recursion
   */ 
  def func: I = recursion(func)
  
  def prependPair(cons: Constructor[C])(arg: cons.pattern.RecDataType) : IndexedRecursiveDefinition = {
    type D = cons.pattern.RecDataType
    
    val caseFn : D => I => I => I =
         (d) => (f) => (g) => cons.pattern.recModify(cons.cons)(d)(f)(g)
    
    IndexedRecDefinitionCons(arg, caseFn, self)
  }
  
  import RecursiveDefinition._
  
  def prepend(cons: Constructor[C], sym: AnySym) = 
    prependPair(cons)(cons.pattern.recDom(W, X).symbObj(sym))
}

/**
 * recursive definition with empty constructor, hence empty data
 */
case class IndexedRecDefinitionTail(W: F, X: Typ[C], index: Ind) extends IndexedRecursiveDefinition{
  def recursion(f : => I) = {
    lazy val ff = typFmlyPtn.fill(f)(index)
    val wtyp = typFmlyPtn.contractType(W)(index)
    typFmlyPtn.iterFuncTyp(wtyp, X).symbObj(TermSymbol(f))
  }
}

case class IndexedRecDefinitionCons[D<: Term with Subs[D]](
    arg: D,
    caseFn : D => I => I => I,
    tail: IndexedRecursiveDefinition) extends IndexedRecursiveDefinition{
  
  lazy val W = tail.W
  
  lazy val X = tail.X
  
  def recursion(f: => I) = {
    def fn = caseFn(arg)(f)(tail.recursion(f))
    fn
  }
  
}

object IndexedRecursiveDefinition{
  
  
  
  def recFn(conss: List[Constructor[C]], W: F, X: Typ[C], index: Ind) = {
    val namedConss = for (c <- conss) yield (c, NameFactory.get)
    
    def addCons(cn :(Constructor[C], String), defn : IndexedRecursiveDefinition) = 
      defn.prepend(cn._1, cn._2)
      
    val init : IndexedRecursiveDefinition = IndexedRecDefinitionTail(W, X, index)
    val lambdaValue : Term = (namedConss :\ init)(addCons).func
    
    val variables : List[Term] = for ((c, name) <- namedConss) yield c.pattern.recDom(W, X).symbObj(name)
    
    (variables :\ lambdaValue)(lmbda(_)(_))
  }
       
}

// Copied from InductiveDefinition

trait IndexedInductiveDefinition{self =>
    /**
   * W in ind(W)(X)
   */
  val W: F

  /**
   * Xs in ind(W)(Xs)
   */
   val Xs : Func[Term, Typ[C]]
  
  /**
   * recursive definition, with offspring applying f.
   */
  def induction(f : => DI) : DI
  
  /**
   * the function to use, applying itself to recursion
   */ 
  def func: DI = induction(func)
  
  def prependPair(cons: Constructor[C])(arg: cons.pattern.InducDataType) : IndexedInductiveDefinition = {
    type D = cons.pattern.InducDataType
    
    val caseFn : D => DI => DI => DI =
         (d) => (f) => (g) => cons.pattern.inducModify(cons.cons)(d)(f)(g)
    
    IndexedInducDefinitionCons(arg, caseFn, self)
  }
  
  import InductiveDefinition._
  
  def prepend(cons: Constructor[C], sym: AnySym) = 
    prependPair(cons)(cons.pattern.inducDom(W, Xs)(cons.cons).symbObj(sym))
}

/**
 * recursive definition with empty constructor, hence empty data
 */
case class IndexedInducDefinitionTail(W: F, Xs: Func[Term, Typ[C]]) extends IndexedInductiveDefinition{
  def induction(f : => DI): DI = 
//    new DepFuncDefn((a: Term) => Xs(a).symbObj(ApplnSym(f, a)), W, Xs)
???
}

case class IndexedInducDefinitionCons[D<: Term with Subs[D]](
    arg: D,
    caseFn : D => DI => DI => DI,
    tail: IndexedInductiveDefinition) extends IndexedInductiveDefinition{
  
  lazy val W = tail.W
  
  lazy val Xs = tail.Xs
  
  def induction(f: => DI) = {
   def fn = caseFn(arg)(f)(tail.induction(f))
    fn
   }
  
}

object IndexedInductiveDefinition{
  
  
  
  def inducFn(conss: List[Constructor[C]], W: F, Xs: Func[Term, Typ[C]]) = {
    val namedConss = for (c <- conss) yield (c, NameFactory.get)
    
    def addCons(cn :(Constructor[C], String), defn : IndexedInductiveDefinition) = 
      defn.prepend(cn._1, cn._2)
      
    val init : IndexedInductiveDefinition = IndexedInducDefinitionTail(W, Xs)
    val lambdaValue : Term = (namedConss :\ init)(addCons).func
    
    val variables : List[Term] = for ((c, name) <- namedConss) yield c.pattern.inducDom(W, Xs)(c.cons).symbObj(name)
    
    (variables :\ lambdaValue)(lmbda(_)(_))
  }
       
      
  
}



}
