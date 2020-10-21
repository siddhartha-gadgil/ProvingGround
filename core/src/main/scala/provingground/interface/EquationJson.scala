package provingground.interface

import provingground._, HoTT._, learning._, translation._, induction._
import ujson._
import provingground.learning.Sort.All
import provingground.learning.Sort.Filter
import provingground.learning.Sort.Restrict
import provingground.learning.TermRandomVars._, TermJson._
import GeneratorVariables._
import provingground.interface.InducJson._
import scala.collection.mutable
import provingground.learning.GeneratorNode.Island

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
    Obj("base" -> randomVarToJson(event.base), "sort" -> sortJson(event.sort))

  def polyJson(value: Any): ujson.Value = value match {
    case t: Term => Obj("type" -> "term", "value" -> termToJson(t).get)
    case fn: ExstFunc =>
      Obj("type" -> "func", "value" -> termToJson(fn.func).get)
    case ind: ExstInducDefn =>
      Obj("type" -> "induc-defn", "value" -> upickle.default.write(ind))
    case ind: ExstInducStrucs =>
      Obj("type" -> "induc-struc", "value" -> InducJson.toJson(ind))
    case v: Vector[u] =>
      Obj(
        "type"  -> "vector",
        "value" -> Arr(v.map(polyJson(_)).to(mutable.ArrayBuffer))
      )
  }

  def jsonPoly(js: ujson.Value): Any = {
    val obj   = js.obj
    val value = obj("value")
    val tp    = obj("type").str
    tp match {
      case "term"       => jsonToTerm()(value).get: Term
      case "func"       => ExstFunc.opt(jsonToTerm()(value).get).get: ExstFunc
      case "induc-defn" => upickle.default.read[ExstInducDefn](value)
      case "induc-struc" =>
        InducJson.fromJson(ExstInducStrucs.Base)(value): ExstInducStrucs
      case "vector" => value.arr.to(Vector).map(jsonPoly(_)): Vector[_]
    }
  }

  def elemJson(el: Elem[_]): Value =
    Obj(
      "random-var" -> randomVarToJson(el.randomVar),
      "element"    -> polyJson(el.element)
    )

  def jsonElem(js: ujson.Value): Elem[_] = {
    val obj = js.obj
    jsonToRandomVar(obj("random-var")) match {
      case rv: RandomVar[u] =>
        Elem[u](polyJson(obj("element")).asInstanceOf[u], rv)
    }
  }

  import TermGeneratorNodes._

  def isleJson[Y, InitState, O, Boat](
      isle: Island[Y, InitState, O, Boat]
  ): ujson.Value = {
    val outputJS = randomVarToJson(isle.output)
    val initMapJS = isle.initMap match {
      case AddVar(typ) =>
        Obj("family" -> "add-var", "type" -> termToJson(typ).get)
    }
    val exportJS = isle.export match {
      case LamApply   => Str("lam-apply")
      case LamFunc    => Str("lam-func")
      case PiApply    => Str("pi-apply")
      case SigmaApply => Str("sigma-apply")
    }

    val isleOutJS = isle.islandOutput match {
      case ConstRandVar(randomVar) =>
        Obj("family" -> "constant", "random-var" -> randomVarToJson(randomVar))
      case PiOutput(pd) =>
        Obj("family" -> "pi-output", "pi-defn" -> termToJson(pd).get)
    }
    Obj(
      "output"        -> outputJS,
      "island-output" -> isleOutJS,
      "init-map"      -> initMapJS,
      "export"        -> exportJS,
      "final-map"     -> Str("enter-isle")
    )
  }

  def jsonIsle(js: ujson.Value): Island[_, TermState, _, Term] = {
    val obj      = js.obj
    val output   = jsonToRandomVar(obj("output"))
    val initMap  = AddVar(toTyp(jsonToTerm()(obj("init-map").obj("type")).get))
    val finalMap = EnterIsle
    val export = obj("export").str match {
      case "lam-apply"   => LamApply
      case "lam-func"    => LamFunc
      case "pi-apply"    => PiApply
      case "sigma-apply" => SigmaApply
    }
    output match {
      case rv: RandomVar[y] =>
        obj("island-output").obj("family").str match {
          case "constant" =>
            val crvBase = jsonToRandomVar(
              obj("island-output").obj("random-var")
            )
            crvBase match {
              case crv: RandomVar[o] =>
                val islandOutput = ConstRandVar(crv)
                Island[y, TermState, o, Term](
                  rv,
                  islandOutput,
                  initMap,
                  export.asInstanceOf[(Term, o) => y],
                  finalMap
                )
            }
          case "pi-output" =>
            jsonToTerm()(obj("island-output").obj("pi-defn")).get match {
              case pd: PiDefn[u, v] =>
                val islandOutput = PiOutput(pd)
                ???
            }
        }

    }

  }

}
