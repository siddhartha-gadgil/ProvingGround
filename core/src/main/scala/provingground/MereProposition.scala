package provingground

import HoTT._

object MereProposition {
  def isPropn[U <: Term with Subs[U]](typ: Typ[U]) = {
    val x = typ.Var
    val y = typ.Var
    pi(x)(pi(y)(x =:= y))
  }

  // Should refine to type of unit
  case class Truncation[U <: Term with Subs[U]](base: Typ[U])
      extends Typ[Term] {
    lazy val typ = base.typ

    def subs(x: Term, y: Term) = Truncation(base.replace(x, y))

    def newobj = Truncation(base.newobj)

    def variable(name: AnySym) = SymbObj(name, this)

    type Obj = Term

    lazy val propWitness = isPropn(this)
  }

  case class Quotient[U <: Term with Subs[U]](base: Typ[U])
      extends Func[U, Term] {
    lazy val dom = base

    lazy val codom = Truncation(base)

    lazy val typ = dom ->: codom

    def act(arg: U): provingground.HoTT.Term =
      codom.symbObj(ApplnSym(this, arg))

    def subs(x: provingground.HoTT.Term, y: provingground.HoTT.Term) =
      Quotient(base.replace(x, y))

    def newobj = Quotient(base.newobj)
  }

  case class Factorize[U <: Term with Subs[U], V <: Term with Subs[V]](
      A: Typ[U], B: Typ[V])
      extends Func[Term, Func[Func[U, V], Func[Term, V]]]
      with Subs[Factorize[U, V]] {

    lazy val dom = isPropn(B)

    lazy val codom = (A ->: B) ->: (Truncation(A) ->: B)

    lazy val typ = (dom: Typ[Term]) ->: codom

    def subs(x: Term, y: Term) = Factorize(A.replace(x, y), B.replace(x, y))

    def newobj = Factorize(A.newobj, B.newobj)

    def act(arg: Term) = codom.symbObj(ApplnSym(this, arg))
  }

  //mainly for testing
  def mere[U <: Term with Subs[U], V <: Term with Subs[V]](fn: Func[U, V]) = {
    val A = fn.dom
    val B = fn.codom
    Factorize(A, Truncation(B))(Truncation(B).propWitness)(fn)
  }
}
