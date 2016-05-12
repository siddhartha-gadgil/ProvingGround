package provingground

import scala.language.implicitConversions
import scala.util._
import scala.language.existentials
import Math._

// To Do:
//
// * A cleaner substitution, parametrised by type.
// * Properly abstracted Formal Function Application - done to some extent
// * Inductive type trait, with Sigma type etc. having this. - done to some extent
//
//

//

/**
 * The Homotopy type theory objects, types and utility functions
 *S
 */
object HoTT {
  /**
   * Symbol
   */
  class AnySym

  /**
   * Strings as symbols
   */
  case class Name(name: String) extends AnySym {
    override def toString = name.toString
  }

  /**
   * use strings as symbols.
   */
  implicit def stringSym(name: String) = Name(name)

  /** Abstract object */
  trait Term extends Subs[Term] {
    /**
     * Gives limited information on types when the types are universes, so should not be used in this case.
     * So avoid making this judgement.
     */
    def typ: Typ[Term]

    /**
     * returns whether this depends on that
     */
    def dependsOn(that: Term) = {
      val newVar = innervar(that)
      replace(that, newVar) != this
    }

    /**
     * returns whether this is independent of that.
     */
    def indepOf(that: Term) = !dependsOn(that)

  }

  /**
   * specify result of substitution, typically so a class is closed under substitution.
   */
  trait Subs[+U <: Term] {
    /**
     *  substitute x by y.
     */
    def subs(x: Term, y: Term): U with Subs[U]

    /**
     * refine substitution so if x and y are both abstract pairs with independent components (for x),
     * both components are substituted.
     * testing for types also done.
     */
    def replace(x: Term, y: Term): U with Subs[U] = {
//      require(x.typ == y.typ, s"cannot replace $x of type ${x.typ} with $y of type ${y.typ}")
      (x, y) match {
        case (ab: AbsPair[u, v], cd: AbsPair[w, x]) if (ab.first indepOf ab.second) && (ab.second indepOf ab.first) =>
          replace(ab.first, cd.first) replace (ab.second, cd.second)
        case (FormalAppln(f, x), FormalAppln(g, y)) =>
          replace(f, g) replace(x, y)
        case _ => subs(x, y)
      }
    }

    def newobj: U with Subs[U]
  }

  /**
   * Objects with simple substitution.
   */
  trait AtomicTerm extends Term with Subs[AtomicTerm] {
    def subs(x: Term, y: Term) = if (x == this) Try(y.asInstanceOf[AtomicTerm]).getOrElse(this) else this

    def newobj = typ.obj.asInstanceOf[AtomicTerm]
  }

  trait ConstantTerm extends Term {
    def subs(x: Term, y: Term) = this

    def newobj = this
  }

  trait ConstantTyp extends Typ[Term] {
    def subs(x: Term, y: Term) = this

    def newobj = this

    def symbObj(name: AnySym) = SymbObj(name, this)

    val typ = Type
  }

  /**
   * HoTT Type;
   *  The compiler knows that objects have scala-type extending U.
   *  In particular, we can specify that the objects are types, functions, dependent functions etc.
   *  @tparam U bound on onjects with this as type.
   */
  trait Typ[+U <: Term with Subs[U]] extends Term with Subs[Typ[U]] { self =>
    /** scala type of objects with this HoTT-type */
    type Obj <: U with Subs[Obj]

    /**
     * factor for producing objects of the given type.
     * can use innervar if one wants name unchanged.
     */
    def obj: U = {
      object newname extends AnySym
      symbObj(newname)
    }

    object Elem {
      def apply(u: Term): Term = u !: self

      def unapply(term: Term): Option[U] =
        if (term.typ == self) Try(Some(term.asInstanceOf[U])).getOrElse(None) else None
    }

    /**
     * checks term is of this type and returns it; useful for documentation.
     */
    def !:(term: Term): U = {
      require(term.typ == this, " Expected " + toString + "but found " + term.typ.toString)
      term.asInstanceOf[U]
    }

    /**
     * type of a type is a universe.
     */
    val typ: Univ

    lazy val typlevel: Int = univlevel(typ)

    /** A symbolic object with this HoTT type, and with scala-type Obj*/
    def symbObj(name: AnySym): U with Subs[U]

    /** Make symbolic object */
    def ::(name: String) = symbObj(name)

    /**
     * new variable
     */
    def Var = getVar(this)

    /**
     * function type:  this -> that
     */
    def ->:[W <: Term with Subs[W], UU >: U <: Term with Subs[UU]](that: Typ[W]) = FuncTyp[W, UU](that, this)

    /**
     * dependent function type (Pi-Type) define by a lambda:
     *  `this` depends on a variable, which hence gives a type family.
     */
    def ~>:[UU >: U <: Term with Subs[UU], V <: Term with Subs[V]](variable: V) = {
      val fiber = LambdaFixed[V, Typ[UU]](variable, this)
      PiTyp(fiber)
    }

    /**
     * returns pair type, mainly to use for "and" for structures
     */
    def &&[UU >: U <: Term with Subs[UU], V <: Term with Subs[V]](
      that: Typ[V]
    ) = PairTyp[UU, V](this, that)

    /**
     * returns Sigma-Type, mainly to use as "such that", for example a group type is this with product etc. dependent on this.
     */
    def ++[UU >: Typ[U] <: Typ[Term] with Subs[UU], VV <: Term with Subs[VV], V <: Typ[VV] with Subs[V]](
      those: V
    ) = SigmaTyp[UU, VV](LambdaFixed[UU, V](
      this,
      those
    ))
  }

  /**
   * symbols for printing
   */
  trait TermSyms {
    val Arrow: String
    val MapsTo: String
    val Pi: String
    val Sigma: String
    val UnivSym: String
  }

  /**
   * simple symbols for maps etc.
   */
  object SimpleSyms extends TermSyms {
    val Arrow = "->"
    val MapsTo = ":->"
    val Pi = "Pi"
    val Sigma = "Sigma"
    val UnivSym = "_"
  }

  /**
    * unicode symbols for maps etc.
   */
  object UnicodeSyms extends TermSyms {
    val Arrow = '\u2192'.toString
    val MapsTo = "\u21A6"
    val Pi = "\u220f"
    val Sigma = "\u2211"
    val UnivSym = "\uD835\uDCB0 "
  }

  //  import SimpleSyms._
  import UnicodeSyms._

  /**
   * terms that are given (and determined) by name;
   *  does not include, for instance, pairs each of whose instance is given by a name;
   *  most useful for pattern matching, where the name contains information about formal function applications etc.
   */
  trait Symbolic {
    val name: AnySym
    override def toString = name.toString
  }

  /**
   * checks if symbolic object has given name.
   */
  def hasName(sym: AnySym): Term => Boolean = {
    case obj: Symbolic =>
      obj.name == sym
    case _ => false
  }

  /**
   *  symbolic objects that are Terms but no more refined
   *  ie, not pairs, formal functions etc.
   */
  case class SymbObj[+U <: Term with Subs[U]](name: AnySym, typ: Typ[U]) extends Term with Symbolic {
    override def toString = name.toString + " : (" + typ.toString + ")"

    def newobj = SymbObj(new InnerSym(this), typ)

    def subs(x: Term, y: Term) = if (x == this) y else {
      def symbobj(sym: AnySym) = typ.replace(x, y).symbObj(sym)
      symSubs(symbobj)(x, y)(name)
    }
  }

  /**
   * substitute symbols, with the only non-trivial substitution for formal applications.
   */
  def symSubs[U <: Term](symbobj: AnySym => U)(x: Term, y: Term): AnySym => U = {
    case fx: ApplnSym[w, u] =>
      Try((fx.func.replace(x, y))(fx.arg.replace(x, y).asInstanceOf[w]).asInstanceOf[U]) getOrElse symbobj(fx)
    case sym => symbobj(sym)
  }

  /**
   * Symbolic types, which the compiler knows are types.
   *
   */
  case class SymbTyp(name: AnySym) extends Typ[Term] with Symbolic {
    lazy val typ = Universe(0)

    def newobj = SymbTyp(new InnerSym(this))

    type Obj = Term

    def symbObj(name: AnySym) = SymbObj(name, this)

    override def toString = s"""${name.toString} : $UnivSym"""

    def elem = this

    //     val applptntypu = ApplnPattern[Term, Typ[Term]]()

    def subs(x: Term, y: Term) = (x, y) match {
      case (u: Typ[_], v: Typ[_]) if (u == this) => v
      case _ => {
        def symbobj(name: AnySym) = SymbTyp(name)
        symSubs(symbobj)(x, y)(name)
      }
    }

  }

  /**
   * Types with symbolic objects not refined.
   */
  trait SmallTyp extends Typ[Term] {
    type Obj = Term

    val typ = Universe(0)

    def symbObj(name: AnySym): Term = SymbObj(name, this)

    def newobj = typ.obj

    def subs(x: Term, y: Term) = (x, y) match {
      case (xt: Typ[_], yt: Typ[_]) if (xt == this) => yt
      case _ => this
    }
  }

  /**
   * Empty type
   */
  case object Zero extends SmallTyp

  /**
   * Unit type.
   */
  case object Unit extends SmallTyp

  /**
   *  the object in the Unit type
   */
  case object Star extends AtomicTerm {
    val typ = Unit
  }

  val One = Unit

  /**
   *  Symbol for map 0 -> A
   */
  case object vacuous extends AnySym

  /**
   * Map from 0 to A.
   */
  def fromZero[U <: Term with Subs[U]](codom: Typ[U]) = (Zero ->: codom).symbObj(vacuous)

  /*
	case class fromZero[U<: Term](codom: Typ[U]) extends AtomicTerm{
      lazy val typ = Zero ->: codom
    }
*/

  /**
   * Notation for universes.
   */
  type Univ = Typ[Typ[Term]]

  trait BaseUniv{
    override def hashCode = Type.hashCode

    override def equals(a: Any) = a match{
      case _ : BaseUniv => true
      case `Type` => true
      case _ => false
    }
  }

  /** The (usual) universes */
  case class Universe(level: Int) extends Univ {
    require(level >= 0)

    type Obj = Typ[Term]

    lazy val typ = Universe(level + 1)

    def symbObj(name: AnySym) = SymbTyp(name)

    def newobj = this

    def subs(x: Term, y: Term) = this

    override def toString = UnivSym

    override def equals(that: Any) = that match {
      case Universe(k) if  k==level => true
      case _ : BaseUniv if level == 0 => true
      case _ => false
      }

  }



  def univlevel: Typ[Typ[Term]] => Int = {
    case Universe(l) => l
    case _ => 0
  }

  /**
   * The first universe
   */
  val Type = Universe(0)

  /** Pair of types (A, B) */
  case class PairTyp[U <: Term with Subs[U], V <: Term with Subs[V]](
    first: Typ[U], second: Typ[V]
  ) extends Typ[PairObj[U, V]] with AbsPair[Typ[U], Typ[V]] with Subs[PairTyp[U, V]]{

    type Obj = PairObj[U, V]

    lazy val typ = Universe(Math.max(first.typlevel, second.typlevel))

    def newobj = {
      val newfirst = first.newobj
      PairTyp(newfirst, second replace(first, newfirst))
    }

    lazy val paircons = {
      val a = first.Var
      val b = second.Var
      lmbda(a)(lmbda(b)(PairObj(a, b)))
    }

    def subs(x: Term, y: Term) : PairTyp[U, V] = if (x == this) Try(
      y.asInstanceOf[PairTyp[U, V]]
    ).getOrElse(
      { println(y); println(x); println(y.typ); PairTyp(first.replace(x, y), second.replace(x, y)) }
    )
    else PairTyp(first.replace(x, y), second.replace(x, y))

    // The name is lost as `name', but can be recovered using pattern matching.
    def symbObj(name: AnySym): Obj = PairObj(first.symbObj(LeftSym(name)), second.symbObj(RightSym(name)))
  }

  /** Object (a, b) in (A, B) */
  case class PairObj[U <: Term with Subs[U], V <: Term with Subs[V]](
    first: U, second: V
  ) extends AbsPair[U, V] with Subs[PairObj[U, V]] {
    lazy val typ: PairTyp[Term, Term] = PairTyp(first.typ, second.typ)

    def newobj =  {
      val newfirst = first.newobj
      PairObj(newfirst, second replace(first, newfirst))
    }

    def subs(x: Term, y: Term) = if (x == this) y.asInstanceOf[PairObj[U, V]] else PairObj[U, V](first.replace(x, y), second.replace(x, y))
  }

  /** Abstract pair, parametrized by scala types of components, generally a (dependent) pair object or a pair type. */
  trait AbsPair[+U <: Term with Subs[U], +V <: Term with Subs[V]] extends Term with Subs[AbsPair[U, V]]{
    val first: U
    val second: V

    override def toString = s"""(($first) , ($second))"""
  }

  object AbsPair {
    def apply(x: Term, y: Term) = mkPair(x, y)

    def unapply(x: Term): Option[(Term, Term)] = x match {
      case ab: AbsPair[_, _] => Some((ab.first, ab.second))
      case _ => None
    }
  }

  object Tuple {
    def apply(xs: Term*): Term = xs.toList match {
      case List() => Star
      case List(x) => x
      case x :: ys => AbsPair(x, apply(ys: _*))
    }

    def asTuple(x: Term): List[Term] = x match {
      case Star => List()
      case AbsPair(x, y) => x :: asTuple(y)
      case _ => List(x)
    }

    def unapplySeq(x: Term): Option[Seq[Term]] =
      if (x == Star) None else Some(asTuple(x))

  }

  /**
   * overloaded method returning a pair object or a pair type.
   */
  def pair[U <: Term with Subs[U], V <: Term with Subs[V]](
    first: U, second: V
  ) = PairObj(first, second)

  /**
   * overloaded method returning a pair object or a pair type.
   */
  def pair[U <: Term with Subs[U], V <: Term with Subs[V]](
    first: Typ[U] with Subs[Typ[U]], second: Typ[V] with Subs[Typ[V]]
  ) = PairTyp(first, second)

  /**
   * makes a pair with the appropriate runtime type
   */
  lazy val mkPair: (Term, Term) => AbsPair[Term, Term] = {
    case (a: Typ[u], b: Typ[v]) => PairTyp[Term, Term](a, b)
    case (a, b) if b.typ.dependsOn(a) => {
      val fiber = lmbda(a)(b.typ)
      DepPair(a, b, fiber)
    }
    case (a, b) => PairObj(a, b)
  }

  /** Function type (not dependent functions)*/
  case class FuncTyp[W <: Term with Subs[W], U <: Term with Subs[U]](dom: Typ[W], codom: Typ[U]) extends Typ[Func[W, U]] with Subs[FuncTyp[W, U]] {
    type Obj = Func[W, U]

    lazy val typ = Universe(max(dom.typlevel, codom.typlevel))

    def symbObj(name: AnySym) = FuncSymb[W, U](name, dom, codom)

    override def toString = s"(${dom.toString}) $Arrow (${codom.toString})"

    def newobj  = {
      val newdom = dom.newobj
      FuncTyp(newdom, codom replace (dom, newdom))
    }


    def subs(x: Term, y: Term) = FuncTyp[W, U](dom.replace(x, y), codom.replace(x, y))
  }

  /**
   * Symbol for domain of a symbolic function
   */
  case class DomSym(func: AnySym) extends AnySym

  /**
   * Symbol for co-domain of a symbolic function
   */
  case class CodomSym(func: AnySym) extends AnySym

  //    case class FnSym(func: AnySym) extends AnySym

  /**
   * Includes both functions and dependent functions
   *
   */
  trait FuncLike[W <: Term with Subs[W], +U <: Term with Subs[U]] extends Term with (W => U) with Subs[FuncLike[W, U]] {
    type Obj <: FuncLike[W, U]

    //    // val domobjtpe : Type

    //    // val codomobjtpe: Type

    val dom: Typ[W]

    val depcodom: W => Typ[U]

    def act(arg: W): U

    def apply(arg: W): U = {
      require(arg.typ == dom, s"function $this with domain ${dom} cannot act on term ${arg} with type ${arg.typ}")
      arg match{
        case t : Cnst => Try(apply(t.term.asInstanceOf[W])).getOrElse(act(arg))
        case _ => act(arg)
      }

      act(arg)
    }
    //      def andThen[WW >: U <: Term, UU <: Term](fn: WW => UU): FuncLike[WW, UU]

    def subs(x: Term, y: Term): FuncLike[W, U]
  }

  /*
     * A symbol representing a formal application
     */
  case class ApplnSym[W <: Term with Subs[W], U <: Term with Subs[U]](func: FuncLike[W, U], arg: W) extends AnySym {
    override def toString = s"""(${func.toString}) (${arg.toString})"""
  }


  /*
   * Pattern matching for a formal application.
  */
  object FormalAppln{
        def unapply(term : Term) : Option[(Term, Term)] = term match {
          case sym : Symbolic => sym.name match {
            case sm : ApplnSym[_,_]  =>
              Some((sm.func, sm.arg))
            case _ => None
          }
          case _ => None
        }

        def apply[U<: Term with Subs[U], V<: Term with Subs[V]](fn: Func[U, V], x: U) =
          fn.codom.symbObj(ApplnSym(fn, x))

        def apply[U<: Term with Subs[U], V<: Term with Subs[V]](fn: FuncLike[U, V], x: U) =
          fn.depcodom(x).symbObj(ApplnSym(fn, x))
      }


  /**
   *  a function (not dependent), i.e.,  an object in a function type, has a codomain and a fixed type for the domain.
   *
   */
  trait Func[W <: Term with Subs[W], +U <: Term with Subs[U]] extends FuncLike[W, U] with Subs[Func[W, U]]{
    /** domain*/
    val dom: Typ[W]
    /** codomain */
    val codom: Typ[U]

    val depcodom: W => Typ[U] = _ => codom

    /** Action, i.e., function application */
    //	  def action(arg:dom.Obj): codom.Obj

    /** Function application */

    def act(arg: W): U

    def subs(x: Term, y: Term): Func[W, U]

  }

  /**
   * wrap function adding name
   */
  case class NamedFunc[W <: Term with Subs[W], +U <: Term with Subs[U]](
    name: AnySym, func: Func[W, U]
  ) extends Func[W, U] {
    lazy val dom = func.dom

    lazy val codom = func.codom

    def typ = func.typ

    def newobj = NamedFunc(name, func.newobj)

    def act(arg: W): U = func.act(arg)

    def subs(x: Term, y: Term) = NamedFunc(name, func.subs(x, y))
  }

  /**
   * wrap dependent function adding name
   */
  case class NamedDepFunc[W <: Term with Subs[W], +U <: Term with Subs[U]](
    name: AnySym, func: FuncLike[W, U]
  ) extends FuncLike[W, U] {
    lazy val dom = func.dom

    lazy val depcodom = func.depcodom

    def typ = func.typ

    def newobj = NamedDepFunc(name, func.newobj)

    def act(arg: W): U = func.act(arg)

    def subs(x: Term, y: Term) = NamedDepFunc(name, func.subs(x, y))
  }

  /** Symbol containing function info */
  case class FuncSymb[W <: Term with Subs[W], U <: Term with Subs[U]](name: AnySym, dom: Typ[W], codom: Typ[U]) extends AnySym with Func[W, U] with Subs[Func[W, U]] with Symbolic {

    //      // val domobjtpe = typeOf[W]

    //	  // val codomobjtpe = typeOf[U]

    lazy val typ = FuncTyp[W, U](dom, codom)

    def act(arg: W): U = codom.symbObj(ApplnSym(this, arg))

    def newobj = FuncSymb(new InnerSym(this), dom, codom)

    def subs(x: Term, y: Term) = (x, y) match {
      //        case (u: Typ[_], v: Typ[_]) => FuncSymb(name, dom.replace(u, v), codom.replace(u, v))
      case (u, v: Func[W, U]) if (u == this) => v
      case _ => {
        def symbobj(sym: AnySym) = FuncSymb(sym, dom.replace(x, y), codom.replace(x, y))
        symSubs(symbobj)(x, y)(name)
      }
    }

    override def toString = s"""${name.toString} : (${typ.toString})"""
  }

  /** A function given by a scala function */
  class FuncDefn[W <: Term with Subs[W], U <: Term with Subs[U]](
    func: => (W => U), val dom: Typ[W], val codom: Typ[U]
  ) extends Func[W, U] {
    //	  // val domobjtpe = typeOf[W]

    //	  // val codomobjtpe = typeOf[U]

    type D = W

    type Cod = U

    lazy val typ = FuncTyp[W, U](dom, codom)

    def act(arg: W) = func(arg)

    def newobj = typ.obj

    def subs(x: Term, y: Term) = new FuncDefn((w) => func(w).replace(x, y), dom.replace(x, y), codom.replace(x, y))
  }

  /**
   *  A lambda-expression.
   *  variable is mapped to value.
   *  This may or may not be a dependent function.
   *  If it is important to note that it is not dependent, and hence has scala type Func, then use LambdaFixed
   *
   */
  sealed trait LambdaLike[X <: Term with Subs[X], Y <: Term with Subs[Y]] extends FuncLike[X, Y] {
    //	  // val domobjtpe = typeOf[X]

    //	  // val codomobjtpe = typeOf[Y]

    val variable: X

    val value: Y

    type D = X

    type Cod = Y

    val dom = variable.typ.asInstanceOf[Typ[X]]

    override def toString = s"""(${variable.toString}) $MapsTo (${value.toString})"""

    val dep: Boolean

    lazy val typ: Typ[FuncLike[X, Y]] = if (dep) {
      val fibre = (t: X) => value.typ subs (variable, t)

      val family: Func[X, Typ[Y]] = LambdaFixed(variable, value.typ.asInstanceOf[Typ[Y]])
      PiTyp(family)
    } else FuncTyp(variable.typ.asInstanceOf[Typ[X]], value.typ.asInstanceOf[Typ[Y]])

    def act(arg: X) = value.replace(variable, arg)

    override def hashCode = {
      val newvar = variable.typ.symbObj(Name("hash"))
      val newval = value.replace(variable, newvar)
      41 * (variable.typ.hashCode + 41) + newval.hashCode
    }

    def subs(x: Term, y: Term) : LambdaLike[X, Y] = Lambda(variable replace (x, y), value replace (x, y))

//    private lazy val myv = variable.newobj

    def andthen[Z <: Term with Subs[Z]](f: Y => Z) = Lambda(variable, f(value))
  }

  /**
   * functions given by lambda, which may be dependent - this is checked by making a substitution.
   */
  case class Lambda[X <: Term with Subs[X], Y <: Term with Subs[Y]](variable: X, value: Y) extends LambdaLike[X, Y] {

    val depcodom: X => Typ[Y] = (t: X) => value.typ.replace(variable, t).asInstanceOf[Typ[Y]]

    val dep = value.typ dependsOn variable

    def newobj = {
      val newvar = variable.newobj
      Lambda(newvar, value.replace(variable, newvar))
    }

    override def equals(that: Any) = that match {
      case Lambda(x: Term, y: Term) if x.typ == variable.typ =>
        (Try(y.replace(x, variable)) map (_ == value)).getOrElse(false)
      case _ => false
    }
  }

  @deprecated("Using function definition", "to be purged")
  class LazyLambda[X <: Term with Subs[X], Y <: Term with Subs[Y]](val variable: X, _value: => Y) extends LambdaLike[X, Y] with Subs[LazyLambda[X, Y]] {

    lazy val value = _value

    lazy val depcodom: X => Typ[Y] = (t: X) => value.typ.replace(variable, t).asInstanceOf[Typ[Y]]

    lazy val dep = true

    def newobj = new LazyLambda(variable.newobj, value.newobj)

    override def subs(x: Term, y: Term): LazyLambda[X, Y] = (x, y) match {
      case (u: Typ[_], v: Typ[_]) if (variable.typ.replace(u, v) != variable.typ) =>
        val newvar = ??? //changeTyp(variable, variable.typ.replace(u, v))
        new LazyLambda(newvar.asInstanceOf[X], value.replace(x, y))
      case _ =>
        val newvar = variable.newobj
        val newval = value.replace(variable, newvar).replace(x, y).replace(newvar, variable) // change variable to avoid name clashes.
        new LazyLambda(variable, newval)
    }
  }

  @deprecated("Using function definition", "to be purged")
  class LazyLambdaFixed[X <: Term with Subs[X], Y <: Term with Subs[Y]](val variable: X, _value: => Y) extends LambdaLike[X, Y] with Func[X, Y] with Subs[LazyLambdaFixed[X, Y]] {

    lazy val value = _value

    //    lazy val depcodom : X => Typ[Y] = (t : X) => value.typ.replace(variable, t).asInstanceOf[Typ[Y]]

    override val dom = variable.typ.asInstanceOf[Typ[X]]

    lazy val codom = value.typ.asInstanceOf[Typ[Y]]

    lazy val dep = false

    def newobj = new LazyLambdaFixed(variable.newobj, value.newobj)

    override def subs(x: Term, y: Term): LazyLambdaFixed[X, Y] = (x, y) match {
      case (u: Typ[_], v: Typ[_]) if (variable.typ.replace(u, v) != variable.typ) =>
        val newvar = ???// changeTyp(variable, variable.typ.replace(u, v))
        new LazyLambdaFixed[X, Y](newvar.asInstanceOf[X], value.replace(x, y))
      case _ =>
        val newvar = variable.newobj
        val newval = value.replace(variable, newvar).replace(x, y).replace(newvar, variable) // change variable to avoid name clashes.
        new LazyLambdaFixed[X, Y](variable, newval)
    }
  }


  /**
   * lambda which is known to have fixed codomain.
   */
  case class LambdaFixed[X <: Term with Subs[X], Y <: Term with Subs[Y]](variable: X, value: Y)
    extends LambdaLike[X, Y] with Func[X, Y] with Subs[LambdaFixed[X, Y]] {
    override val dom = variable.typ.asInstanceOf[Typ[X]]

    val codom = value.typ.asInstanceOf[Typ[Y]]

    val dep = false

    override def equals(that: Any) = that match {
      case LambdaFixed(x: Term, y: Term) if (x.typ == variable.typ)=>
        (Try(y.replace(x, variable)) map (_ == value)).getOrElse(false)
      case _ => false
    }

    def newobj = {
      val newvar = variable.newobj
      LambdaFixed(newvar, value.replace(variable, newvar))
    }

    override def subs(x: Term, y: Term): LambdaFixed[X, Y] =
      LambdaFixed(variable replace (x, y), value replace (x, y))

  }

  /**
   * term as a symbol
   */
  case class TermSymbol(term: Term) extends AnySym

  def termSymbol(term: Term): AnySym = TermSymbol(term)

  /**
   * returns symbolic object with new type.
   */
//  def changeTyp[U <: Term with Subs[U]](term: U, newtyp: Typ[U]): U = term match {
//    case sym: Symbolic => newtyp.symbObj(sym.name)
//    case _ => newtyp.symbObj(term)
//  }

  /**
   * instantiates variable values in a term if it is made from lambdas, to give a term (if possible) of a required HoTT type.
   * @param target required type after instantiation
   * @param substitutions substitutions to make.
   */
  def instantiate(substitutions: Term => Option[Term], target: Typ[Term]): Term => Option[Term] = {
    case t: Term if t.typ == target => Some(t)
    case Lambda(variable: Term, value: Term) =>
      substitutions(variable) flatMap ((cnst) => {
        val reduced = (value.replace(variable, cnst))
        instantiate(substitutions, target)(reduced)
      })
    case _ => None
  }

  /**
   * A symbol to be used to generate new variables of a type, with string matching given variable.
   */
  class InnerSym[U <: Term](val variable: U) extends AnySym {
    override def toString = variable match {
      case sym: Symbolic => sym.name.toString
      case x => x.toString
    }
  }

  /**
   * variable of given type with string as in given variable.
   */
  private def innervar[U <: Term with Subs[U]](variable: U): U = {
    val typ = variable.typ.asInstanceOf[Typ[U]]
    val newvar = new InnerSym(variable)
    variable match {
      case PairObj(a: Term, b: Term) => PairObj(
        a.typ.symbObj(newvar),
        b.typ.symbObj(newvar)
      ).asInstanceOf[U]
      case PairTyp(a: Term, b: Term) => PairTyp(
        a.typ.symbObj(newvar),
        b.typ.symbObj(newvar)
      ).asInstanceOf[U]
      case DepPair(a: Term, b: Term, fibre) => DepPair[Term, Term](
        a.typ.symbObj(newvar),
        b.typ.symbObj(newvar), fibre.asInstanceOf[TypFamily[Term, Term]]
      ).asInstanceOf[U]
      case _ => typ.symbObj(newvar)
    }

  }

  /**
   * Lambda constructor
   *
   */
  def lambda[U <: Term with Subs[U], V <: Term with Subs[V]](variable: U)(value: V): FuncLike[U, V] = {
    val newvar = variable.newobj
    if (value.typ dependsOn variable) Lambda(newvar, value.replace(variable, newvar)) else LambdaFixed(newvar, value.replace(variable, newvar))
  }

  /**
   * lambda constructor for fixed codomain
   */
  def lmbda[U <: Term with Subs[U], V <: Term with Subs[V]](variable: U)(value: V): Func[U, V] = {
    require(
      value.typ.indepOf(variable),
      s"lambda returns function type but value $value has type ${value.typ} depending on variable $variable"
    )
    val newvar = variable.newobj
    LambdaFixed(newvar, value.replace(variable, newvar))
  }

  def id[U <: Term with Subs[U]](typ: Typ[U]) = {
    val x = typ.Var
    lmbda(x)(x)
  }

  def pi[U <: Term with Subs[U], V <: Term with Subs[V]](variable: U)(value: Typ[V]) : Typ[FuncLike[U, V]] =
    if (value dependsOn variable) PiTyp(lmbda(variable)(value)) else (variable.typ.asInstanceOf[Typ[U]] ->: value)

  /**
   * lambda if necessary, otherwise constant.
   */
  def optlambda(variable: Term): Term => Term = value =>
    if (value dependsOn variable) lambda(variable)(value) else value

  def lambdaPair[U <: Term with Subs[U], V <: Term with Subs[V]](
    variable: U
  )(value: V) = {
    val fibre = lmbda(variable)(value.typ.asInstanceOf[Typ[V]])
    DepPair(variable, value, fibre)
  }

    def composition[U<: Term with Subs[U], V<: Term with Subs[V], W<: Term with Subs[W]](f: Func[V, W], g: Func[U, V]) = {
      val x = g.dom.Var
      LambdaFixed(x, f(g(x)))
    }

  /**
   * Sigma type based on lambda
   *
   */
  def sigma[U <: Term with Subs[U], V <: Term with Subs[V]](
    variable: U
  )(value: Typ[V]) : Typ[AbsPair[U, V]] =
    if (value.dependsOn(variable)) {
      val fibre = lmbda(variable)(value)
      SigmaTyp(fibre)
      }
    else PairTyp(variable.typ.asInstanceOf[Typ[U]], value)


  /**
   * type family
   */
  type TypFamily[W <: Term with Subs[W], +U <: Term with Subs[U]] = Func[W, Typ[U]]

  /**
   *  For all/Product for a type family. This is the type of dependent functions
   */
  case class PiTyp[W <: Term with Subs[W], U <: Term with Subs[U]](fibers: TypFamily[W, U]) extends Typ[FuncLike[W, U]] with Subs[PiTyp[W, U]] {
    //type Obj = DepFunc[W, U]
    type Obj = FuncLike[W, U]

    lazy val typ = Universe(max(univlevel(fibers.codom), univlevel(fibers.dom.typ)))

    override def symbObj(name: AnySym) = DepFuncSymb[W, U](name, fibers)

    def newobj = PiTyp(fibers.newobj)

    def subs(x: Term, y: Term) = PiTyp[W, U](fibers.replace(x, y))

    override def toString = Pi + "(" + fibers.toString + ")"
  }

  /**
   *  Object in a dependent function type, i.e.,
   *  a dependent function. Has a family of codomains
   */
  trait DepFunc[W <: Term with Subs[W], U <: Term with Subs[U]] extends FuncLike[W, U] {
    val fibers: TypFamily[W, U]

    type D = W

    type Cod = U

    def act(arg: W): U

  }

  /**
   * Symbolic dependent function
   */
  case class DepFuncSymb[W <: Term with Subs[W], U <: Term with Subs[U]](
    name: AnySym, fibers: TypFamily[W, U]
  ) extends AnySym with DepFunc[W, U] with Symbolic {
    //	  // val domobjtpe = typeOf[W]

    //	  // val codomobjtpe = typeOf[U]

    // type D = W

    // type Cod = U

    val dom = fibers.dom

    val depcodom: W => Typ[U] = (arg: W) => fibers(arg)

    lazy val typ = PiTyp(fibers)

    def act(arg: W) = fibers(arg).symbObj(ApplnSym(this, arg))

    def newobj = DepFuncSymb(new InnerSym(this), fibers.newobj)

    /*
	  def subs(x: Term, y: Term) = (x, y, name) match {
        case (u: Typ[_], v: Typ[_], _) => DepFuncSymb(name, fibers.replace(u, v))
        case (u, v: FuncLike[W, U], _) if (u == this) => v
        case _ => this
      }
      *
      */

    def subs(x: Term, y: Term) = (x, y) match {
      //        case (u: Typ[_], v: Typ[_]) => FuncSymb(name, dom.replace(u, v), codom.replace(u, v))
      case (u, v: FuncLike[W, U]) if (u == this) => v
      case _ => {
        def symbobj(sym: AnySym) = DepFuncSymb(sym, fibers.replace(x, y))
        symSubs(symbobj)(x, y)(name)
      }
    }

    override def toString = s"""${name.toString} : (${typ.toString})"""
  }

  /** A dependent function given by a scala funcion */
  class DepFuncDefn[W <: Term with Subs[W], U <: Term with Subs[U]](
    func: W => U, val dom: Typ[W], val fibers: TypFamily[W, U]
  ) extends DepFunc[W, U] {
    //	  // val domobjtpe = typeOf[W]

    //	  // val codomobjtpe = typeOf[U]

    val depcodom: W => Typ[U] = (arg: W) => fibers(arg)

    lazy val typ = PiTyp[W, U](fibers)

    //	  def act(arg: W) = if (arg.typ == dom) Some(func(arg)) else None

    def act(arg: W) = func(arg)

    def newobj = typ.obj

    def subs(x: Term, y: Term) = new DepFuncDefn((w: W) => func(w).replace(x, y), dom.replace(x, y), fibers.replace(x, y))
  }

  case class OptDepFuncDefn[W <: Term with Subs[W]](
    func: W => Option[Term], dom: Typ[W]
  ) extends DepFunc[W, Term] with Subs[OptDepFuncDefn[W]] {

    lazy val depcodom = (arg: W) => (func(arg) map (_.typ)).getOrElse(Unit)

    lazy val fibers = {
      val x = getVar(dom)
      lmbda(x)(depcodom(x))
    }

    lazy val typ = PiTyp(fibers)

    def act(arg: W) = func(arg).getOrElse(Star)

    def newobj = this

    def subs(x: Term, y: Term) =
      OptDepFuncDefn((w: W) => func(w) map (_.replace(x, y)), dom.replace(x, y))
  }

  /**
   *  symbol for left component in pair from a given symbol
   */
  case class LeftSym(name: AnySym) extends AnySym {
    override def toString = name.toString + "_1"
  }

  /**
   *  symbol for right component in pair from a given symbol
   */
  case class RightSym(name: AnySym) extends AnySym {
    override def toString = name.toString + "_2"
  }

  /**
   *  Exists/Sum for a type family
   */
  case class SigmaTyp[W <: Term with Subs[W], U <: Term with Subs[U]](
    fibers: TypFamily[W, U]
  ) extends Typ[DepPair[W, U]] {
    lazy val typ = Universe(max(univlevel(fibers.codom), univlevel(fibers.dom.typ)))

    type Obj = DepPair[W, U]

    def symbObj(name: AnySym) = {
      val a = fibers.dom.symbObj(LeftSym(name))
      val b = fibers(a).symbObj(RightSym(name))
      DepPair(a, b, fibers)
    }

    lazy val paircons = {
      val a = fibers.dom.Var
      val b = (fibers(a)).Var
      lambda(a)(lmbda(b)(DepPair(a, b, fibers)))
    }

    def newobj = SigmaTyp(fibers.newobj)

    def subs(x: Term, y: Term) = SigmaTyp[W, U](fibers.replace(x, y))

    override def toString = Sigma + "(" + fibers.toString + ")"
  }

  object Sgma {
    def apply[W <: Term with Subs[W], U <: Term with Subs[U]](variable: W, typ: Typ[U]) = {
      val fibers = lmbda(variable)(typ)
      SigmaTyp(fibers)
    }
  }

  /**
   * Dependent pair (a: A, b : B(a)) - element of a Sigma type.
   *
   */
  case class DepPair[W <: Term with Subs[W], U <: Term with Subs[U]](first: W, second: U, fibers: TypFamily[W, U]) extends Term with Subs[DepPair[W, U]] with AbsPair[W, U] {
    lazy val typ = SigmaTyp(fibers)

    def newobj = {
      val newfirst = first.newobj
      DepPair(newfirst, second.replace(first, newfirst), fibers.replace(first, newfirst))
    }

    def subs(x: Term, y: Term) = if (x == this) y.asInstanceOf[DepPair[W, U]] else DepPair(first.replace(x, y), second.replace(x, y), fibers.replace(x, y))
  }

  /**
   * The identity type.
   *  This is the type lhs = rhs
   */
  case class IdentityTyp[+U <: Term with Subs[U]](dom: Typ[U], lhs: U, rhs: U) extends Typ[Term] with Subs[IdentityTyp[U]] {
    type Obj = Term

    lazy val typ = Universe(max(univlevel(lhs.typ.typ), univlevel(rhs.typ.typ)))

    def newobj = {
      val newlhs = lhs.newobj
      IdentityTyp(dom replace (lhs, newlhs), newlhs , rhs replace (lhs, newlhs))
    }

    def subs(x: Term, y: Term) = IdentityTyp(dom.replace(x, y), lhs.replace(x, y), rhs.replace(x, y))

    def symbObj(name: AnySym) = SymbObj(name, this)
  }

  case class Refl[U <: Term with Subs[U]](dom: Typ[U], value: U) extends AtomicTerm {
    lazy val typ = IdentityTyp(dom, value, value)
  }


  implicit class RichTerm[U <: Term with Subs[U]](term: U) {

    def =:=(rhs: U) = {
      require(term.typ == rhs.typ, "mismatched types for equality " + term.typ + " and " + rhs.typ)
      IdentityTyp(term.typ.asInstanceOf[Typ[U]], term, rhs)
    }

    def :->[V <: Term with Subs[V]](that: V) = lmbda(term)(that)

    def :~>[V <: Term with Subs[V]](that: V) = lambda(term)(that)
  }

  object IdentityTyp{
    case class RecFunc[U <: Term with Subs[U], V <: Term with Subs[V]](
        dom: Typ[U], target : Typ[V]) extends AnySym

    def rec[U <: Term with Subs[U], V <: Term with Subs[V]](
        dom: Typ[U], target : Typ[V]) = {
      val baseCase = dom ->: target
      val x = dom.Var
      val y = dom.Var
      val resultTyp = x ~>: y ~>: (IdentityTyp(dom, x, y) ->: target)
      (baseCase ->: resultTyp).symbObj(RecFunc(dom, target))
    }

    case class InducFunc[U <: Term with Subs[U], V <: Term with Subs[V]](
        dom: Typ[U], targetFmly : FuncLike[U, FuncLike[U, Func[Term, Typ[V]]]]) extends AnySym

    def induc[U <: Term with Subs[U], V <: Term with Subs[V]](
        dom: Typ[U], targetFmly : FuncLike[U, FuncLike[U, Func[Term, Typ[V]]]]) = {
      val x = dom.Var
      val y = dom.Var
      val p = IdentityTyp(dom, x, y).Var
      val baseCaseTyp = x ~>: (targetFmly(x)(x)(Refl(dom, x)))
      val resultTyp = x ~>: y ~>: p~>: (IdentityTyp(dom, x, y) ->: targetFmly(x)(y)(p))
      (baseCaseTyp ->: resultTyp).symbObj(InducFunc(dom, targetFmly))
    }

    def symm[U<: Term with Subs[U]](dom: Typ[U]) = {
      val x = dom.Var
      val y = dom.Var
      val p = IdentityTyp(dom, x, y).Var
      val typFamily = lambda(x)(lambda(y)(lmbda(p)(IdentityTyp(dom, y, x))))
      val inducFn = induc(dom, typFamily)
      val baseCase = lambda(x)(id(x =:= x))
      inducFn(baseCase)
    }

    def trans[U<: Term with Subs[U]](dom: Typ[U]) = {
      val x = dom.Var
      val y = dom.Var
      val z = dom.Var
      val p = IdentityTyp(dom, x, y).Var
      val typFamily = lmbda(x)(lmbda(y)(lmbda(p)((x =:= y) ->: (y =:= z) ->: (x =:= z) )))
      val inducFn = induc(dom, typFamily)
      val q = (x =:= x).Var
      val baseCase = lambda(x)(lmbda(q)(id(x =:= z)))
      lambda(z)(inducFn(baseCase))
    }

    def extnslty[U <: Term with Subs[U], V<: Term with Subs[V]](f: Func[U, V]) = {
      val x = f.dom.Var
      val y = f.dom.Var
      val p = IdentityTyp(f.dom, x, y).Var
      val typFamily = lmbda(x)(lmbda(y)(lmbda(p)( (f(x) =:= f(y)) )))
      val inducFn = induc(f.dom, typFamily)
      val image = Refl(f.codom, f(x)) : Term
      val baseCase = lambda(x)(image)
      inducFn(baseCase)
    }
  }

  //	implicit def richTerm(term: Term with Subs[Term]) = RichTerm(term)

  //	implicit def richTyp(typ: Typ[Term] with Subs[Typ[Term]]) = RichTerm(typ)


  object PlusTyp {
    /**
     * A -> A + B
     */
    case class FirstIncl[U<: Term with Subs[U], V <: Term with Subs[V]](
        typ: PlusTyp[U, V], value: U) extends Term with Subs[FirstIncl[U, V]] {

      def newobj = this

      def subs(x: Term, y: Term) = FirstIncl(typ, value.replace(x, y))
    }

    /**
     * B -> A + B
     */
    case class ScndIncl[U<: Term with Subs[U], V <: Term with Subs[V]](
        typ: PlusTyp[U, V], value: V) extends Term with Subs[ScndIncl[U, V]] {
      def newobj = this

      def subs(x: Term, y: Term) = ScndIncl(typ, value.replace(x, y))
    }
  }

  /**
   * type A + B
   */
  case class PlusTyp[U<: Term with Subs[U], V <: Term with Subs[V]](
      first: Typ[U], second: Typ[V]) extends SmallTyp {plustyp =>
    def i(value: U) = PlusTyp.FirstIncl(this, value)

    def j(value: V) = PlusTyp.ScndIncl(this, value)

    val ifn = {
      val a = first.Var
      lmbda(a)(i(a))
    }

    val jfn = {
      val a = second.Var
      lmbda(a)(j(a))
    }

    case class Rec[W<: Term with Subs[W]](
        codom: Typ[W],
        firstCase: Func[U, W],
        secondCase: Func[V, W]) extends Func[Term, W]{
      def act(x: Term) = x match {
        case PlusTyp.FirstIncl(`first`, y) =>
          firstCase(y.asInstanceOf[U])
        case PlusTyp.ScndIncl(`second`, y) =>
          secondCase(y.asInstanceOf[V])
        case _ =>
          codom.symbObj(ApplnSym(this, x))
      }

      lazy val typ = dom ->: codom

      def subs(x: Term, y: Term) = this

      val dom: provingground.HoTT.Typ[provingground.HoTT.Term] = plustyp

      def newobj = this
    }

    case class Induc[W<: Term with Subs[W]](
        depcodom: Func[Term, Typ[W]],
        firstCase: FuncLike[U, W],
        secondCase: FuncLike[V, W]) extends FuncLike[Term, W]{
      def act(x: Term) = x match {
        case PlusTyp.FirstIncl(`first`, y) =>
          firstCase(y.asInstanceOf[U])
        case PlusTyp.ScndIncl(`second`, y) =>
          secondCase(y.asInstanceOf[V])
        case _ =>
          depcodom(x).symbObj(ApplnSym(this, x))
      }

      lazy val typ = PiTyp(depcodom)

      def subs(x: Term, y: Term) = this

      val dom: provingground.HoTT.Typ[provingground.HoTT.Term] = plustyp

      def newobj = this
    }

  }



  /**
   * folds in as many terms of the list as possible,
   * applying terms as long as the result is a function and the list is non-empty.
   */
  def foldterms: (Term, List[Term]) => Term = {
    case (f: FuncLike[u, _], x :: ys) if f.dom == x.typ =>
      foldterms(f(x.asInstanceOf[u]), ys)
    case (t, _) => t
  }

  def fold(fn: Term)(args: Term*): Term = (fn, args.toList) match {
    case (t, List()) => t
    case (f: FuncLike[u, _], x :: ys) if f.dom == x.typ =>
      fold(f(x.asInstanceOf[u]))(ys: _*)
    case (t, _) =>
      throw new IllegalArgumentException(
        s"attempting to apply $t, which is not a function"
      )
  }

  object Fold{
   implicit class Folder[U <: Term with Subs[U]](fn: U){
    def apply(args: Term*) = fold(fn)(args : _*)
  }
  }

  /**
   * folds in as many terms with names given by the list as possible,
   * applying terms as long as the result is a function and the list is non-empty.
   */
  def foldnames: (Term, List[AnySym]) => Term = {
    case (f: FuncLike[u, _], x :: ys) =>
      val newvar = f.dom.symbObj(x)
      foldnames(f(newvar.asInstanceOf[u]), ys)
    case (t, _) => t
  }

  /** Symbol factory */
  def nextChar(s: Traversable[Char]) = if (s.isEmpty) 'a' else (s.max + 1).toChar

  /** Helper for symbol factory */
  def usedChars(s: Traversable[Term]): Traversable[Char] = {
    def charOpt(obj: Term): Option[Char] = obj match {
      case sym: Symbolic => Try(sym.name.asInstanceOf[Char]).toOption
      case _ => None
    }

    s collect (Function.unlift(charOpt _))
  }

  def nextVar(s: Traversable[Term])(typ: Typ[Term]) = {
    typ.symbObj(Name(nextChar(usedChars(s)).toString))
  }

  def nextName(name: String): String = {
    if (name == "") "a"
    else if (name.takeRight(1) == "z") nextName(name.dropRight(1)) + "a"
    else (name.dropRight(1)) + (name.toCharArray.last + 1).toChar.toString
  }

  object NameFactory {
    var name = ""

    def get = {
      name = nextName(name)
      "$"+name
    }
  }

  def getVar[U <: Term with Subs[U]](typ: Typ[U]) = typ.symbObj(NameFactory.get)


  def asLambdas[U <: Term with Subs[U]](term: U) : Option[U] = term match {
    case LambdaFixed(x: Term, y : Term) =>
      for (z <- asLambdas(y); w <- Try(lmbda(x)(z).asInstanceOf[U]).toOption) yield w
    case Lambda(x: Term, y : Term) =>
      for (z <- asLambdas(y); w <- Try(lambda(x)(z).asInstanceOf[U]).toOption) yield w
    case fn : Func[u, v] => {
      val x = fn.dom.Var
      val y = fn(x)
      Try(lmbda(x)(y).asInstanceOf[U]).toOption flatMap (asLambdas)
    }
    case fn : FuncLike[u, v] => {
      val x = fn.dom.Var
      val y = fn(x)
      Try(lambda(x)(y).asInstanceOf[U]).toOption flatMap (asLambdas)
    }
    case _ => None
  }


  /**
   * Just a wrapper to allow singleton objects
   */
  class Cnst(val term: Term) extends Term{
//    val value = term

    val typ = term.typ

    def subs(x: Term, y: Term) = this

    def newobj = this
  }

  class CnstFunc[U <: Term with Subs[U], V <: Term with Subs[V]](override val term : Func[U, V]) extends Cnst(term) with Func[U, V]{
//    override val value : Func[U, V] = term

    def act(x: U) = term(x)

    val dom = term.dom

    val codom = term.codom

    override def newobj = this

    override def subs(x: Term, y: Term) = this
  }

  class CnstFuncLike[U <: Term with Subs[U], V <: Term with Subs[V]](override val term : FuncLike[U, V]) extends Cnst(term) with FuncLike[U, V]{
//    override val value:

    def act(x: U) = term(x)

    val dom = term.dom

    val depcodom = term.depcodom

    override def newobj = this

    override def subs(x: Term, y: Term) = this
  }

  // -----------------------------------------------
  // Deprecated code - old style type families.

  object Deprec {
    /**
     * returns type family, but needs a universe specified as the codomain.
     */
    def typFamilyDefn[W <: Term with Subs[W], U <: Term with Subs[U]](dom: Typ[W], codom: Typ[Typ[U]], f: W => Typ[U]) = {
      new FuncDefn[W, Typ[U]](f, dom, codom)
    }

    case class MiniVerse[U <: Term with Subs[U]](sample: Typ[U]) extends Typ[Typ[U]] {
      type Obj = Typ[U]

      lazy val typ = MiniVerse[Typ[U]](this)

      def symbObj(name: AnySym) = sample

      def newobj = this

      def subs(x: Term, y: Term) = this
    }
  }

}
