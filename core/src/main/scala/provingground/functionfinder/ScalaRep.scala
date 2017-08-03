package provingground.functionfinder
import provingground._, HoTT._
// import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv}

import scala.util._

import scala.language.implicitConversions
import scala.language.existentials
//import provingground.ScalaUniv._

/**
 * Representation by a scala object of a HoTT term
 *
 * It is assumed that there is a single HoTT type corresponding to the given scala type,
 * when the scala rep is in scope. If one needs several HoTT types, eg. vectors of different lengths,
 * one uses ScalaPolyRep.
 *
 * ScalaRep objects are recursively constructed from
 *
 * @tparam U the HoTT type represented
 * @tparam V scala type representing the given object.
 */
trait ScalaRep[+U <: Term with Subs[U], V] {

  /**
   * HoTT type represented.
   */
  val typ: Typ[U]

  /**
   * scala type of representing object.
   */
  type tpe = V

  /**
   * HoTT object from the scala one.
   */
  def apply(v: V): U

  /**
   * pattern for matching application of the scala object.
   */
  def unapply(u: Term): Option[V]

  def subs(x: Term, y: Term): ScalaRep[U, V]

  import ScalaRep._

  /**
   * scalarep for function
   */
  def -->:[W <: Term with Subs[W], X, UU >: U <: Term with Subs[UU]](
    that: ScalaRep[W, X]) =
    FuncRep[W, X, UU, V](that, this)

  def :-->[W <: Term with Subs[W], UU >: U <: Term with Subs[UU]](
    that: Typ[W]) =
    SimpleFuncRep[UU, V, W](this, that)

  /**
   *   scalarep for sum type.
   */
  def ++[UU >: U <: Term with Subs[UU], X <: Term with Subs[X], Y](
    codrepfmly: V => ScalaRep[X, Y]) = SigmaRep[UU, V, X, Y](this, codrepfmly)
}

import ScalaRep._

trait RepTerm[A] extends Term with Subs[RepTerm[A]] {
  val typ: Typ[RepTerm[A]]
}

class ScalaTyp[A] extends Typ[RepTerm[A]] {
  type Obj = RepTerm[A]

  val typ = ScalaTypUniv[A]

  def variable(name: AnySym): RepTerm[A] =
    RepSymbObj[A, RepTerm[A]](name, this)

  def newobj =
    throw new IllegalArgumentException(
      s"trying to use the constant $this as a variable (or a component of one)")

  def subs(x: Term, y: Term) = (x, y) match {
    case (xt: Typ[_], yt: Typ[_]) if (xt == this) =>
      yt.asInstanceOf[Typ[RepTerm[A]]]
    case _ => this
  }

  implicit val rep: ScalaRep[RepTerm[A], A] = SimpleRep(this)
}

case class SymbScalaTyp[A](name: AnySym) extends ScalaTyp[A] with Symbolic {
  override def subs(x: Term, y: Term) = (x, y) match {
    case (u: Typ[_], v: Typ[_]) if (u == this) =>
      v.asInstanceOf[Typ[RepTerm[A]]]
    case _ => {
      def symbobj(name: AnySym) = SymbScalaTyp[A](name.subs(x, y))
      symSubs(symbobj)(x, y)(name)
      // SymbScalaTyp(name.subs(x, y))
    }
  }
}

import ScalaUniv._

case class ScalaTypUniv[A]() extends Typ[Typ[RepTerm[A]]] with BaseUniv {
  lazy val typ = HigherUniv(this)

  type Obj = Typ[RepTerm[A]]

  def subs(x: Term, y: Term) = this

  def newobj =
    throw new IllegalArgumentException(
      s"trying to use the constant $this as a variable (or a component of one)")

  def variable(name: AnySym) = SymbScalaTyp[A](name)
}

object ScalaRep {

  /**
   * A term representing itself.
   */
  case class IdRep[U <: Term with Subs[U]](typ: Typ[U])
    extends ScalaRep[U, U] {
    def apply(v: U) = v

    def unapply(u: Term): Option[U] = u match {
      case t: Term if t.typ == typ => Some(t.asInstanceOf[U])
      case _ => None
    }

    def subs(x: Term, y: Term) = IdRep(typ.subs(x, y))
  }

  case class IdTypRep[U <: Term with Subs[U]]()(implicit univ: Typ[Typ[U]])
    extends ScalaRep[Typ[U], Typ[U]] {
    val typ = univ

    def apply(v: Typ[U]) = v

    def unapply(u: Term): Option[Typ[U]] =
      Try(u.asInstanceOf[Typ[U]]).toOption

    def subs(x: Term, y: Term) = this
  }

  /**
   * Representations for functions given ones for the domain and codomain.
   */
  case class FuncRep[U <: Term with Subs[U], V, X <: Term with Subs[X], Y](
    domrep: ScalaRep[U, V],
    codomrep: ScalaRep[X, Y]) extends ScalaRep[Func[U, X], V => Y] {
    lazy val typ = domrep.typ ->: codomrep.typ

    def apply(f: V => Y): Func[U, X] = ExtendedFunction(f, domrep, codomrep)

    def unapply(u: Term): Option[V => Y] = u match {
      case ext: ExtendedFunction[_, V, _, Y] if ext.domrep == domrep && ext.codomrep == codomrep =>
        Some(ext.dfn)
      case _ => None
    }

    def opt(f: V => Option[Y]) = {
      def optfn(u: U) = u match {
        case domrep(v) => f(v) map (codomrep(_))
        case _ => None
      }
      OptDepFuncDefn(optfn, domrep.typ)
    }

    def subs(x: Term, y: Term) =
      FuncRep(domrep.subs(x, y), codomrep.subs(x, y))
  }

  object SimpleFuncRep {
    def apply[U <: Term with Subs[U], V, X <: Term with Subs[X]](
      domrep: ScalaRep[U, V],
      codom: Typ[X]) = FuncRep(domrep, IdRep(codom))
  }

  /**
   * Function rep with codomain representing itself. Should perhaps use  IdRep instead.
   */
  case class SmpleFuncRep[U <: Term with Subs[U], V, X <: Term with Subs[X]](
    domrep: ScalaRep[U, V],
    codom: Typ[X]) extends ScalaRep[FuncLike[U, X], V => X] {
    val typ = domrep.typ ->: codom

    def newobj = typ.obj

    def apply(f: V => X) = SimpleExtendedFunction(f, domrep, codom)

    def unapply(u: Term): Option[V => X] = u match {
      case ext: SimpleExtendedFunction[_, V, X] if ext.domrep == domrep && ext.codom == codom =>
        Some(ext.dfn)
      case _ => None
    }

    def subs(x: Term, y: Term) =
      SimpleFuncRep(domrep.subs(x, y), codom.subs(x, y))
  }

  case class SigmaRep[U <: Term with Subs[U], V, X <: Term with Subs[X], Y](
    domrep: ScalaRep[U, V],
    codrepfmly: V => ScalaRep[X, Y]) extends ScalaRep[Term, (V, Y)] {

    //    val rep = SimpleFuncRep(domrep, Type)

    val rep = FuncRep(domrep, Type)

    val fibers = rep((v: V) => codrepfmly(v).typ)

    lazy val typ = SigmaTyp(fibers)

    def apply(vy: (V, Y)) =
      DepPair(domrep(vy._1), codrepfmly(vy._1)(vy._2), fibers)

    def unapply(u: Term) = u match {
      case DepPair(domrep(v), vy, _) =>
        val codrep = codrepfmly(v)
        vy match {
          case codrep(y) => Some((v, y))
          case _ => None
        }
      case _ => None
    }

    def subs(x: Term, y: Term) =
      SigmaRep(domrep.subs(x, y), (v: V) => codrepfmly(v).subs(x, y))
  }

  /**
   * Formal extendsion of a dependent function given scalareps for the domain and codomains.
   */
  case class DepFuncRep[U <: Term with Subs[U], V, X <: Term with Subs[X], Y](
    domrep: ScalaRep[U, V],
    codomreps: V => ScalaRep[X, Y],
    fibers: TypFamily[U, X]) extends ScalaRep[FuncLike[U, X], V => Y] {
    val typ = PiDefn(fibers)

    def apply(f: V => Y): FuncLike[U, X] =
      ExtendedDepFunction(f, domrep, codomreps, fibers)

    def unapply(u: Term): Option[V => Y] = u match {
      case ext: ExtendedDepFunction[_, V, _, Y] if ext.domrep == domrep && ext.codomreps == codomreps =>
        Some(ext.dfn)
      case _ => None
    }

    def subs(x: Term, y: Term) =
      DepFuncRep(
        domrep.subs(x, y),
        (v: V) => codomreps(v).subs(x, y),
        fibers.subs(x, y))
  }

  implicit val UnivRep = idRep(Type)

  implicit def scalaUnivRep[A] = idRep(ScalaTypUniv[A])

  implicit def idRep[U <: Term with Subs[U]](typ: Typ[U]): ScalaRep[U, U] =
    IdRep(typ)

  implicit class ScalaTerm[U <: Term with Subs[U], W](elem: W)(
    implicit
    rep: ScalaRep[U, W]) {
    def term = rep(elem)
  }

  implicit class TermScala[U <: Term with Subs[U]](term: U) {
    type Rep[W] = ScalaRep[U, W]
    def as[W: Rep] = implicitly[ScalaRep[U, W]].unapply(term)
  }

  implicit def funcRep[U <: Term with Subs[U], V, X <: Term with Subs[X], Y](
    implicit
    domrep: ScalaRep[U, V],
    codomrep: ScalaRep[X, Y]): ScalaRep[Func[U, X], V => Y] =
    FuncRep(domrep, codomrep)

  case class RepSymbObj[A, +U <: RepTerm[A] with Subs[U]](
    name: AnySym,
    typ: Typ[U])
    extends RepTerm[A]
    with Symbolic {
    override def toString = name.toString + " : (" + typ.toString + ")"

    def newobj = RepSymbObj(new InnerSym[RepTerm[A]](this), typ)

    def subs(x: Term, y: Term) =
      if (x == this) y.asInstanceOf[RepTerm[A]]
      else {
        def symbobj(sym: AnySym) = typ.replace(x, y).symbObj(sym.subs(x, y))
        symSubs(symbobj)(x, y)(name)
      }
  }

  case object NatInt extends ScalaTyp[Int]

  import NatInt.rep

  // import induction.coarse._
  //
  // implicit val boolRep: ScalaRep[Term, Boolean] = SimpleRep(
  //   BaseConstructorTypes.SmallBool)

  def incl[U <: Term with Subs[U], V, W]: (ScalaRep[U, V], ScalaRep[U, W]) => Option[V => W] = {
    case (x, y) if x == y => Some((v: V) => v.asInstanceOf[W])
    case (rep: ScalaRep[U, V], IdRep(typ)) if rep.typ == typ =>
      Some((v: V) => rep(v).asInstanceOf[W])
    case (fst: FuncRep[_, a, _, b], scnd: FuncRep[_, c, _, d]) => {
      val dmap = incl(scnd.domrep, fst.domrep)
      val cmap = incl(fst.codomrep, scnd.codomrep)
      val m = for (dm <- dmap; cm <- cmap)
        yield ((f: a => b) => (x: c) => cm(f(dm(x))))
      m flatMap
        ((x: (a => b) => (c => d)) => Try(x.asInstanceOf[V => W]).toOption)
    }

    case _ => None
  }

  /**
   * constant  term.
   */
  trait ConstTerm[T] extends Term {
    val value: T

    type scalaType = T

    def newobj = typ.obj

    def subs(x: Term, y: Term) = if (x == this) y else this
  }

  /**
   * formal extension of a function.
   * XXX must check type.
   */
  def extend[T, U <: Term with Subs[U]](
    fn: T => U,
    FuncLike: FuncLike[Term, U],
    codom: Typ[U]): Term => U = {
    case c: ConstTerm[_] =>
      Try(fn(c.value.asInstanceOf[T]))
        .getOrElse(codom.symbObj(ApplnSym(FuncLike, c)))
    case arg: Term => codom.symbObj(ApplnSym(FuncLike, arg))
  }

  /**
   * scala object wrapped to give a HoTT constant with type declared.
   */
  case class SimpleConst[V](value: V, typ: Typ[Term]) extends ConstTerm[V] {
    override def toString = value.toString
  }

  case class ScalaSymbol[X](value: X) extends AtomicSym

  case class SimpleRep[U <: Term with Subs[U], V](typ: Typ[U])
    extends ScalaRep[U, V] {
    def apply(v: V) = typ.symbObj(ScalaSymbol(v))

    def unapply(u: Term) = u match {
      case sym: Symbolic if u.typ == typ =>
        sym.name match {
          case ScalaSymbol(value) => Try(value.asInstanceOf[V]).toOption
          case _ => None
        }
      case _ => None
    }

    def subs(x: Term, y: Term) = SimpleRep(typ.subs(x, y))
  }

  class ScalaSym[U <: Term with Subs[U], V](typ: Typ[U]) {
    def apply(v: V) = typ.symbObj(ScalaSymbol(v))

    def unapply(u: Term): Option[V] = u match {
      case sym: Symbolic if u.typ == typ =>
        sym.name match {
          case ScalaSymbol(value) => Try(value.asInstanceOf[V]).toOption
          case _ => None
        }
      case _ => None
    }
  }

  /**
   * Formal extension of a function given by a definition and representations for
   * domain and codomain.
   */
  case class ExtendedFunction[U <: Term with Subs[U], V, X <: Term with Subs[X], Y](
    dfn: V => Y,
    domrep: ScalaRep[U, V],
    codomrep: ScalaRep[X, Y]) extends Func[U, X] {

    lazy val dom = domrep.typ

    lazy val codom = codomrep.typ

    lazy val typ = dom ->: codom

    def newobj = typ.obj

    def act(u: U) = u match {
      case domrep(v) => codomrep(dfn(v))
      case _ => codom.symbObj(ApplnSym(this, u))
    }

    // val domobjtpe: reflect.runtime.universe.Type = typeOf[U]

    // val codomobjtpe: reflect.runtime.universe.Type = typeOf[X]

    def subs(x: provingground.HoTT.Term, y: provingground.HoTT.Term) =
      (x, y) match {
        case (u, v: Func[U, X]) if u == this => v
        case _ =>
          ExtendedFunction(
            (v: V) => dfn(v),
            domrep.subs(x, y),
            codomrep.subs(x, y))
      }
  }

  /**
   * Extended function with codomain a type. Perhaps use IdRep.
   */
  case class SimpleExtendedFunction[U <: Term with Subs[U], V, X <: Term with Subs[X]](
    dfn: V => X,
    domrep: ScalaRep[U, V],
    codom: Typ[X]) extends Func[U, X]
    with Subs[SimpleExtendedFunction[U, V, X]] {

    val dom = domrep.typ

    val typ = dom ->: codom

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def act(u: U) = u match {
      case domrep(v) => dfn(v)
      case _ => codom.symbObj(ApplnSym(this, u))
    }

    // val domobjtpe: reflect.runtime.universe.Type = typeOf[U]

    // val codomobjtpe: reflect.runtime.universe.Type = typeOf[X]

    def subs(
      x: provingground.HoTT.Term,
      y: provingground.HoTT.Term): SimpleExtendedFunction[U, V, X] =
      (x, y) match {
        case (u, v: SimpleExtendedFunction[U, V, X]) if u == this => v
        case _ =>
          SimpleExtendedFunction(
            (v: V) => dfn(v).subs(x, y),
            domrep.subs(x, y),
            codom.subs(x, y))
      }
  }

  /**
   * implicit class associated to a type family to create dependent functions scalareps.
   */
  implicit class FmlyReps[U <: Term with Subs[U], X <: Term with Subs[X]](
    fibers: TypFamily[U, X]) {

    def ~~>:[V](domrep: ScalaRep[U, V]) = {
      DepFuncRep(domrep, (v: V) => IdRep(fibers(domrep(v))), fibers)
    }
  }

  /**
   * implicit class associated to a family of scalareps to create dependent functions scalareps.
   * Not much use since U ends up having strange bounds such as Term with Long.
   */
  implicit class RepSection[V, X <: Term with Subs[X], Y](
    section: V => ScalaRep[X, Y]) {

    def ~~>:[U <: Term with Subs[U]](domrep: ScalaRep[U, V]) /*(implicit
        sux : ScalaUniv[X], suu: ScalaUniv[U])*/ = {
      val univrep = domrep -->: Type
      val fmly = univrep((v: V) => section(v).typ)
      val x = "x" :: domrep.typ

      val fibers = lmbda(x)(fmly(x))
      //typFamily(domrep.typ, fmly)

      DepFuncRep(domrep, (v: V) => section(v), fibers)
    }
  }

  /**
   * formal extension of a dependent function.
   */
  case class ExtendedDepFunction[U <: Term with Subs[U], V, X <: Term with Subs[X], Y](
    dfn: V => Y,
    domrep: ScalaRep[U, V],
    codomreps: V => ScalaRep[X, Y],
    fibers: TypFamily[U, X]) extends FuncLike[U, X] {

    val dom = domrep.typ

    val depcodom: U => Typ[X] = (arg: U) => fibers(arg)

    val typ = PiDefn(fibers)

    def newobj = typ.obj

    def act(u: U) = u match {
      case domrep(v) => codomreps(v)(dfn(v))
      case arg => fibers(arg).symbObj(ApplnSym(this, arg))
    }

    // val domobjtpe: reflect.runtime.universe.Type = typeOf[Term]

    // val codomobjtpe: reflect.runtime.universe.Type = typeOf[U]

    def subs(x: provingground.HoTT.Term, y: provingground.HoTT.Term) =
      (x, y) match {
        case (u, v: FuncLike[U, X]) if u == this => v
        case _ => this
      }
  }

  object dsl {
    def i[V](typ: Typ[Term]) = SimpleRep[Term, V](typ)

    def s[U <: Term with Subs[U], V, X <: Term with Subs[X], Y](
      domrep: ScalaRep[U, V])(
      codrepfmly: V => ScalaRep[X, Y]) = SigmaRep(domrep, codrepfmly)
  }
}
