package provingground.interface

import provingground._, HoTT._, learning._, translation._, induction._
import ujson._
import provingground.learning.Sort.All
import provingground.learning.Sort.Filter
import provingground.learning.Sort.Restrict
import provingground.learning.TermRandomVars._, TermJson._
import GeneratorVariables._

object EquationJson {
  def sortJson[S, T](sort: Sort[S, T]): ujson.Value =
    sort match {
      case All() => Obj("sort-type" -> "all")
      case Filter(pred) =>
        val predJson = pred match {
          case WithTyp(typ) =>
            Obj("filter" -> "with-typ", "typ" -> termToJson(typ).get)
          case FuncWithDomFilter(dom) =>
            Obj("filter" -> "func-with-dom", "dom" -> termToJson(dom).get)
        }
        Obj("sort-type" -> "filter", "pred" -> predJson)
      case Restrict(optMap) =>
        val optMapJson = optMap match {
          case FuncOpt      => Obj("restrict" -> "func-opt")
          case TypFamilyOpt => Obj("restrict" -> "typ-family-opt")
          case FuncWithDom(dom) =>
            Obj("restrict" -> "func-with-dom", "dom" -> termToJson(dom).get)
          case TypAsTermOpt => Obj("restrict" -> "typ-as-term-opt")
        }
        Obj("sort-type" -> "restrict", "opt-map" -> optMapJson)
    }

  def jsonEvent(js: ujson.Value): Event[_, _] = {
    val obj               = js.obj
    def termAt(s: String) = jsonToTerm()(obj(s)).get
    def typAt(s: String)  = toTyp(termAt(s))
    val rv                = jsonToRandomVar(obj("base"))
    (obj("sort").obj("sort-type").str) match {
      case "restrict" =>
        obj("restrict").str match {
          case "func-opt" =>
            Event[Term, ExstFunc](
              rv.asInstanceOf[RandomVar[Term]],
              Sort.Restrict(FuncOpt)
            )
          case "typ-family-opt" =>
            Event[Term, ExstFunc](
              rv.asInstanceOf[RandomVar[Term]],
              Sort.Restrict(TypFamilyOpt)
            )
          case "func-with-dom" =>
            Event[Term, ExstFunc](
              rv.asInstanceOf[RandomVar[Term]],
              Sort.Restrict(FuncWithDom(typAt("dom")))
            )
          case "typ-as-term-opt" =>
            Event[Typ[Term], Term](
              rv.asInstanceOf[RandomVar[Typ[Term]]],
              Sort.Restrict(TypAsTermOpt)
            )
        }
      case "filter" =>
        obj("filter").str match {
          case "with-typ" =>
            Event[Typ[Term], Typ[Term]](
              rv.asInstanceOf[RandomVar[Typ[Term]]],
              Sort.Filter(WithTyp(typAt("typ")))
            )
          case "func-with-dom" =>
            Event[ExstFunc, ExstFunc](
              rv.asInstanceOf[RandomVar[ExstFunc]],
              Sort.Filter(FuncWithDomFilter(typAt("dom")))
            )
        }
    }
  }

  def eventJson[S, T](event: Event[S, T]): ujson.Value = 
    Obj("base" -> randomVarToJson(event.base), 
        "sort" -> sortJson(event.sort))

}
