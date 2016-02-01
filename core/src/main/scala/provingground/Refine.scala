package provingground

import HoTT._

object RefineTerms {
  def refine(term: Term): Term = term match {
    case LambdaFixed(variable: Term, value: Term) =>
      val newvar = refine(variable)
      val newval = refine(value)
      val vartyp = newvar.typ
      val valtyp = newval.typ
      LambdaFixed[vartyp.Obj, valtyp.Obj](newvar.asInstanceOf[vartyp.Obj], newval.asInstanceOf[valtyp.Obj])
    case Lambda(variable: Term, value: Term) =>
      val newvar = refine(variable)
      val newval = refine(value)
      val vartyp = newvar.typ
      val valtyp = newval.typ
      Lambda[vartyp.Obj, valtyp.Obj](newvar.asInstanceOf[vartyp.Obj], newval.asInstanceOf[valtyp.Obj])
    case sym: Symbolic =>
      refineTyp(sym.typ).symbObj(sym.name)
    case _ => term
  }

  def refineTyp(typ: Typ[Term]): Typ[Term] = typ match {
    case FuncTyp(dom : Typ[Term], codom: Typ[Term]) =>
      val newdom = refineTyp(dom)
      val newcod = refineTyp(codom)
      FuncTyp[newdom.Obj, newcod.Obj](newdom.asInstanceOf[Typ[newdom.Obj]], newcod.asInstanceOf[Typ[newcod.Obj]])
    case _ => typ
  }
}
