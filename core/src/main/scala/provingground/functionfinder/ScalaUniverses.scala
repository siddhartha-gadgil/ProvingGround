package provingground

import HoTT._

/**
  * Wrapper for universe with refined scala type for objects (i.e., types) in it.
  * Refined scala types typically recursively built from (dependent) function types and types of already refined types.
  */
case class ScalaUniv[U <: Term with Subs[U]](univ: Typ[Typ[U]])

object ScalaUniv{

/**
  * given a universe with objects of scala type Typ[U], gives one with scala type Typ[Typ[U]]
  */
case class HigherUniv[U <: Typ[Term] with Subs[U]](univ: Typ[U])
    extends Typ[Typ[U]] {
  type Obj = Typ[U]

  lazy val typ = HigherUniv[Typ[U]](this)

  def variable(name: AnySym) = univ

  def newobj =
    throw new IllegalArgumentException(
      s"trying to use the constant $this as a variable (or a component of one)")

  def subs(x: Term, y: Term) = this
}

/**
  * Universe whose elements are FuncTyps
  */
case class FuncTypUniv[W <: Term with Subs[W], U <: Term with Subs[U]](
    domuniv: Typ[Typ[W]],
    codomuniv: Typ[Typ[U]]
) extends Typ[FuncTyp[W, U]] {

  type Obj = FuncTyp[W, U]

  lazy val typ = HigherUniv(this)

  def variable(name: AnySym) = {
    val dom   = domuniv.symbObj(DomSym(name))
    val codom = codomuniv.symbObj(CodomSym(name))
    FuncTyp(dom, codom)
  }

  def newobj = FuncTypUniv(domuniv.newobj, codomuniv.newobj)

  def subs(x: Term, y: Term) = this
}

/**
  * Universe with objects Pi-Types
  */
@deprecated("Use PiDefn", "14/12/2016")
case class PiTypUniv[W <: Term with Subs[W], U <: Term with Subs[U]](
    domuniv: Typ[Typ[W]],
    codomuniv: Typ[Typ[U]]
) extends Typ[PiTyp[W, U]] {

  type Obj = PiTyp[W, U]

  lazy val typ = HigherUniv(this)

  def variable(name: AnySym) = {
    val dom     = domuniv.symbObj(DomSym(name))
    val codom   = codomuniv.symbObj(CodomSym(name))
    val typFmly = FuncTyp(dom, codomuniv).symbObj(name)
    PiTyp(typFmly)
  }

  def newobj =
    throw new IllegalArgumentException(
      s"trying to use the constant $this as a variable (or a component of one)")

  def subs(x: Term, y: Term) = this
}

/**
  * Symbolic types, which the compiler knows are types.
  *
  */
case class FineSymbTyp[U <: Term with Subs[U]](name: AnySym,
                                               symobj: AnySym => U)
    extends Typ[U]
    with Symbolic {
  lazy val typ = FineUniv(symobj)

  def newobj = FineSymbTyp(new InnerSym[Typ[U]](this), symobj)

  type Obj = U

  def variable(name: AnySym) = symobj(name)

  def elem = this

  def subs(x: Term, y: Term) = (x, y) match {
    case (u: Typ[_], v: Typ[_]) if (u == this) => v.asInstanceOf[Typ[U]]
    case _ => {
      def symbobj(name: AnySym) = FineSymbTyp(name, symobj)
      symSubs(symbobj)(x, y)(name)
    }
  }
}

case class FineUniv[U <: Term with Subs[U]](symobj: AnySym => U)
    extends Typ[Typ[U]]
    with BaseUniv {
  type Obj = Typ[U]

  lazy val typ = HigherUniv(this)

  def variable(name: AnySym) = FineSymbTyp(name, symobj)

  def newobj =
    throw new IllegalArgumentException(
      s"trying to use the constant $this as a variable (or a component of one)")

  def subs(x: Term, y: Term) = this
}

  // import ScalaUniv._

  /**
    * scala universe with no refinement.
    */
  implicit val baseUniv: ScalaUniv[Term] = ScalaUniv(Type)

  /**
    * implicitly returns from a scala universe of Typ[U] one of Typ[Typ[U]]
    */
  implicit def higherUniv[U <: Term with Subs[U]](
      implicit sc: ScalaUniv[U]): ScalaUniv[Typ[U]] = {
    ScalaUniv(HigherUniv(sc.univ))
  }

  def newobj =
    throw new IllegalArgumentException(
      s"trying to use the constant $this as a variable (or a component of one)")

  /**
    * implicitly build universe with elements FuncTyps from universes for domain and codomain.
    */
  implicit def funcUniv[W <: Term with Subs[W], U <: Term with Subs[U]](
      implicit domsc: ScalaUniv[W],
      codomsc: ScalaUniv[U]): ScalaUniv[Func[W, U]] = {
    ScalaUniv(FuncTypUniv(domsc.univ, codomsc.univ): Typ[FuncTyp[W, U]])
  }

  /**
    * builds scala universe for pi-types given ones for domain and codomain types.
    */
  @deprecated("Use PiDefn", "14/12/2016")
  implicit def piUniv[W <: Term with Subs[W], U <: Term with Subs[U]](
      implicit domsc: ScalaUniv[W],
      codomsc: ScalaUniv[U]): ScalaUniv[FuncLike[W, U]] = {
    ScalaUniv(PiTypUniv(domsc.univ, codomsc.univ): Typ[PiTyp[W, U]])
  }

  /**
    * returns dependent function inferring type fiber.
    */
  def depFunc[W <: Term with Subs[W], U <: Term with Subs[U]](dom: Typ[W],
                                                              func: W => U)(
      implicit su: ScalaUniv[U]
  ): FuncLike[W, U] = {
    val fibers = typFamily(dom, (w: W) => func(w).typ.asInstanceOf[Typ[U]])
    new DepFuncDefn(func, dom, fibers)
  }

  /**
    * convenience for Pi-type
    */
  implicit class RichTypFamily[W <: Term with Subs[W], U <: Term with Subs[U]](
      fibre: Func[W, Typ[U]]
  )(
      implicit su: ScalaUniv[U]
  ) {
    //    val dom = func.dom

    def pi = PiTyp(fibre)
  }

  /**
    * Companion to dependent functions
    *
    */
  object DepFunc {

    def apply[W <: Term with Subs[W], U <: Term with Subs[U]](
        func: Term => U,
        dom: Typ[W])(implicit su: ScalaUniv[U]) = {
      def section(arg: Term)      = func(arg).typ.asInstanceOf[Typ[U]]
      val fibers: TypFamily[W, U] = typFamily[W, U](dom, section)
      new DepFuncDefn(func, dom, fibers)
    }
  }

  /**
    * create type family, implicitly using a scala-universe object to build the codomain.
    */
  def typFamily[W <: Term with Subs[W], U <: Term with Subs[U]](
      dom: Typ[W],
      f: W => Typ[U])(implicit su: ScalaUniv[U]) = {
    val codom = su.univ
    new FuncDefn[W, Typ[U]](f, dom, codom)
  }
}
