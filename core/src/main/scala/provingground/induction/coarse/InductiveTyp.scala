package provingground.induction.coarse

import provingground._, HoTT._

trait InductiveTyp[C <: Term with Subs[C], H <: Term with Subs[H]]
    extends Typ[H] {
  val constructors: List[Constructor[C, H]]

  def rec(X: Typ[C]) = ConstructorSeq.recFn(constructors, this, X)

  def induc(Xs: Func[H, Typ[C]]) =
    ConstructorSeq.inducFn(constructors, this, Xs)

  /**
    * just the constructor patterns.
    */
  val ptns = constructors map (_.pattern)

  /**
    * just the constructor functions
    */
  val constructorFns: List[Term] = constructors map (_.cons)
}

object InductiveTyp {
  def fromFormal(formalCons: List[Term], formalTyp: Typ[Term]) = {
    val constructorDefs =
      formalCons map
        ((cons) =>
          (typ: Typ[Term]) => Constructor.fromFormal(cons, formalTyp)(typ))
    InductiveTypDefinition(constructorDefs)
  }
}

case class InductiveTypDefinition[C <: Term with Subs[C]](
    constructorDefs: List[Typ[Term] => Constructor[C, Term]])
    extends InductiveTyp[C, Term] {
  val typ = Type

  lazy val constructors = constructorDefs map (_(this))

  def subs(x: Term, y: Term) = this

  def newobj =
    throw new IllegalArgumentException(
      s"trying to use the constant $this as a variable (or a component of one)")

  /**
    * As seen from class InductiveTypDefinition, the missing signatures are as follows.
    *  *  For convenience, these are usable as stub implementations.
    */
  type Obj = Term

  def variable(name: provingground.HoTT.AnySym) =
    SymbObj(name, this)

  def withCod[CC <: Term with Subs[CC]] =
    InductiveTypDefinition(
      constructorDefs map ((fn) => (t: Typ[Term]) => (fn(t).withCod[CC])))

  def recFn[CC <: Term with Subs[CC]](X: Typ[CC]) =
    withCod[CC].rec(X)

  def inducFn[CC <: Term with Subs[CC]](Xs: Func[Term, Typ[CC]]) =
    withCod[CC].induc(Xs)
}
