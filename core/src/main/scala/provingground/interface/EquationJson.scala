package provingground.interface

import provingground._, HoTT.{Variable => _, _}, learning._, translation._,
induction._
import ujson._
import provingground.learning.Sort.All
import provingground.learning.Sort.Filter
import provingground.learning.Sort.Restrict
import provingground.learning.TermRandomVars._, TermJson._
import GeneratorVariables._
import provingground.interface.InducJson._
import scala.collection.mutable
import provingground.learning.GeneratorNode.Island
import provingground.learning.GeneratorNode.Init
import provingground.learning.GeneratorNode.Atom
import provingground.learning.GeneratorNode.MapOpt
import provingground.learning.GeneratorNode.ZipMap
import provingground.learning.GeneratorNode.ZipMapOpt
import provingground.learning.GeneratorNode.FiberProductMap
import provingground.learning.GeneratorNode.ZipFlatMap
import provingground.learning.GeneratorNode.FlatMap
import provingground.learning.GeneratorNode.FlatMapOpt
import provingground.learning.GeneratorNode.BaseThenCondition
import provingground.learning.GeneratorNode.RecursiveThenCondition
import provingground.learning.GeneratorNode.Idty

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

  def variableJson[Y](v: Variable[Y]): ujson.Value = v match {
    case ev @ Event(base, sort) =>
      Obj("type" -> "event", "value" -> eventJson(ev))
    case InIsle(isleVar, boat: Term, isle) =>
      Obj(
        "type"     -> "in-isle",
        "isle-var" -> variableJson(isleVar),
        "boat"     -> termToJson(boat).get,
        "isle"     -> isleJson(isle)
      )
    case PairEvent(base1, base2, sort) =>
      throw new Exception("trying to serialize pair events, not expected")
    case elem @ Elem(element, randomVar) =>
      Obj("type" -> "elem", "value" -> elemJson(elem))
    case _ => throw new Exception(s"cannot serialize $v")
  }

  def jsonVariable(js: ujson.Value): Variable[_] =
    js.obj("type").str match {
      case "event" => jsonEvent(js.obj("value"))
      case "elem"  => jsonElem(js.obj("value"))
      case "in-isle" =>
        val boat = jsonToTerm()(js.obj("boat")).get
        jsonVariable(js.obj("isle-var")) match {
          case isleVar: Variable[y] =>
            jsonIsle(js.obj("isle")) match {
              case isle: Island[yy, TermState, o, Term] =>
                InIsle[y, yy, TermState, o, Term](isleVar, boat, isle)
            }
        }
    }

  def fromList[A](l: List[A])(s: String) =
    l.find(_.toString() == s)
      .getOrElse(
        throw new Exception(
          s"expected one of ${l.mkString("[", " ,", "]")} but got $s"
        )
      )

  def fromSeq[A](xs: A*)(s: String) = fromList(xs.toList)(s)

  def genNodeJson[X](gen: GeneratorNode[X]): Value = gen match {
    case Init(input) => Obj("type" -> "init", "input" -> randomVarToJson(input))
    case Atom(value, output) =>
      Obj(
        "type"   -> "atom",
        "value"  -> polyJson(value),
        "output" -> randomVarToJson(output)
      )
    case provingground.learning.GeneratorNode.Map(f, input, output) =>
      Obj(
        "type"   -> "map",
        "f"      -> Str(f.toString()),
        "input"  -> randomVarToJson(input),
        "output" -> randomVarToJson(output)
      )
    case MapOpt(f: TargetInducFuncs, input, output) =>
      Obj(
        "type"   -> "map-opt",
        "family" -> "target-induc-funcs",
        "typ"    -> termToJson(f.target).get,
        "f"      -> Str(f.toString()),
        "input"  -> randomVarToJson(input),
        "output" -> randomVarToJson(output)
      )
    case MapOpt(f: TargetCodomain, input, output) =>
      Obj(
        "type"   -> "map-opt",
        "family" -> "target-codomain",
        "typ"    -> termToJson(f.typ).get,
        "f"      -> Str(f.toString()),
        "input"  -> randomVarToJson(input),
        "output" -> randomVarToJson(output)
      )
    case MapOpt(f, input, output) =>
      Obj(
        "type"   -> "map-opt",
        "f"      -> Str(f.toString()),
        "input"  -> randomVarToJson(input),
        "output" -> randomVarToJson(output)
      )
    case ZipMap(ptt: PTTerm[u, v], input1, input2, output) =>
      Obj(
        "type"   -> "zip-map",
        "family" -> Str("ptterm"),
        "pt"     -> termToJson(ptt.pt),
        "input1" -> randomVarToJson(input1),
        "input2" -> randomVarToJson(input2),
        "output" -> randomVarToJson(output)
      )

    case ZipMap(f, input1, input2, output) =>
      Obj(
        "type"   -> "zip-map",
        "family" -> Str("single"),
        "f"      -> Str(f.toString()),
        "input1" -> randomVarToJson(input1),
        "input2" -> randomVarToJson(input2),
        "output" -> randomVarToJson(output)
      )
    case ZipMapOpt(f, input1, input2, output) =>
      Obj(
        "type"   -> "zip-map-opt",
        "f"      -> Str(f.toString()),
        "input1" -> randomVarToJson(input1),
        "input2" -> randomVarToJson(input2),
        "output" -> randomVarToJson(output)
      )
    case FiberProductMap(quot, fiberVar, f, baseInput, output) =>
      Obj(
        "type"       -> "fiber-product-map",
        "quot"       -> Str(quot.toString()),
        "fiber-var"  -> Str(fiberVar.toString()),
        "f"          -> Str(f.toString()),
        "base-input" -> randomVarToJson(baseInput),
        "output"     -> randomVarToJson(output)
      )
    case ZipFlatMap(baseInput, fiberVar: STFibVar[u, v], f, output) =>
      Obj(
        "type"       -> "zip-flat-map",
        "family"     -> "st-fibvar",
        "typ"        -> termToJson(fiberVar.pt).get,
        "base-input" -> randomVarToJson(baseInput),
        "fiber-var"  -> Str(fiberVar.toString()),
        "f"          -> Str(f.toString()),
        "output"     -> randomVarToJson(output)
      )
    case ZipFlatMap(baseInput, fiberVar, f, output) =>
      Obj(
        "type"       -> "zip-flat-map",
        "family"     -> "single",
        "base-input" -> randomVarToJson(baseInput),
        "fiber-var"  -> Str(fiberVar.toString()),
        "f"          -> Str(f.toString()),
        "output"     -> randomVarToJson(output)
      )
    case FlatMap(baseInput, fiberNode, output) =>
      Obj(
        "type"       -> "flat-map",
        "base-input" -> randomVarToJson(baseInput),
        "fiber-node" -> Str(fiberNode.toString()),
        "output"     -> randomVarToJson(output)
      )
    case FlatMapOpt(
        baseInput,
        fiberNodeOpt: TermGeneratorNodes[_]#TargetInducFuncsFolded,
        output
        ) =>
      Obj(
        "type"           -> "flat-map-opt",
        "family"         -> "target-induc-funcs-folded",
        "typ"            -> termToJson(fiberNodeOpt.target).get,
        "base-input"     -> randomVarToJson(baseInput),
        "fiber-node-opt" -> Str(fiberNodeOpt.toString()),
        "output"         -> randomVarToJson(output)
      )
    case FlatMapOpt(baseInput, fiberNodeOpt, output) =>
      Obj(
        "type"           -> "flat-map-opt",
        "family"         -> "single",
        "base-input"     -> randomVarToJson(baseInput),
        "fiber-node-opt" -> Str(fiberNodeOpt.toString()),
        "output"         -> randomVarToJson(output)
      )
    case BaseThenCondition(gen, output, condition) =>
      Obj(
        "type"   -> "base-then-condition",
        "gen"    -> genNodeJson(gen),
        "output" -> randomVarToJson(output)
      )
    case RecursiveThenCondition(gen, output, condition) =>
      Obj(
        "type"   -> "recursive-then-condition",
        "gen"    -> genNodeJson(gen),
        "output" -> randomVarToJson(output)
      )
    case isle @ Island(output, islandOutput, initMap, export, finalMap) =>
      Obj("type" -> "island", "island" -> isleJson(isle))
  }

  def jsonGenNode(
      js: Value,
      tgn: TermGeneratorNodes[TermState]
  ): GeneratorNode[_] = {
    val obj           = js.obj
    def rv(s: String) = jsonToRandomVar(obj(s))
    obj("type").str match {
      case "init" => Init(jsonToRandomVar(obj("input")))
      case "atom" =>
        rv("output") match {
          case output: RandomVar[u] =>
            Atom(jsonPoly(obj("value")).asInstanceOf[u], output)
        }
      case "map" =>
        (rv("input"), rv("output")) match {
          case (input: RandomVar[u], output: RandomVar[v]) =>
            val f: Any = obj("f").str match {
              case "Identity" => Idty[u]()
              case "Negate"   => Negate
              case "GetFunc"  => ExstFunc.GetFunc
            }
            GeneratorNode.Map(f.asInstanceOf[u => v], input, output)
        }
      case "map-opt" =>
        obj("family").str match {
          case "target-induc-funcs" =>
            val typ = toTyp(jsonToTerm()(obj("typ")).get)
            MapOpt[ExstInducDefn, Term](
              TargetInducFuncs(typ),
              rv("input").asInstanceOf[RandomVar[ExstInducDefn]],
              rv("output").asInstanceOf[RandomVar[Term]]
            )
          case "target-codomain" =>
            val typ = toTyp(jsonToTerm()(obj("typ")).get)
            MapOpt[ExstFunc, Term](
              TargetCodomain(typ),
              rv("input").asInstanceOf[RandomVar[ExstFunc]],
              rv("output").asInstanceOf[RandomVar[Term]]
            )
        }
      case "zip-map" =>
        val pt = toTyp(jsonToTerm()(obj("pt")).get) match {
          case pd: ProdTyp[x, y] => pd
        }
        ZipMap[Term, Term, Term](
          PTTerm(pt),
          termsWithTyp(pt.first),
          termsWithTyp(pt.second),
          termsWithTyp(pt)
        )
      case "zip-map-opt" =>
        (rv("input1"), rv("input2"), rv("output")) match {
          case (
              input1: RandomVar[u1],
              input2: RandomVar[u2],
              output: RandomVar[v]
              ) =>
            val f = tgn.UnifApplnOpt
            ZipMapOpt(
              f.asInstanceOf[(u1, u2) => Option[v]],
              input1,
              input2,
              output
            )
        }
      case "fiber-product-map" =>
        obj("fiber-var").str match {
          case "TermsWithTypFn" =>
            FiberProductMap[ExstFunc, Term, Typ[Term], Term](
              DomFn,
              TermsWithTypFn,
              tgn.Appln,
              rv("base-input").asInstanceOf[RandomVar[ExstFunc]],
              Terms
            )
          case "FuncsWithDomainFn" =>
            FiberProductMap[Term, ExstFunc, Typ[Term], Term](
              TypFn,
              FuncsWithDomainFn,
              tgn.FlipAppln,
              Terms,
              Terms
            )
        }
      case "zip-flat-map" =>
        val pt = jsonToTerm()(obj("typ")).get match {
          case st: SigmaTyp[u, v] => st
        }
        ZipFlatMap[Term, Term, Term](
          termsWithTyp(pt.fibers.dom),
          STFibVar(pt),
          STTerm(pt),
          termsWithTyp(pt)
        )
      case "flat-map-opt" =>
        val typ = toTyp(jsonToTerm()(obj("typ")).get)
        FlatMapOpt[ExstInducDefn, Term](
          InducDefns,
          tgn.TargetInducFuncsFolded(typ),
          termsWithTyp(typ)
        )
      case "flat-map" =>
        val fiberNode = {
          import tgn._
          fromSeq(
            LambdaIsle,
            PiIsle,
            RecFuncsFolded,
            InducFuncsFolded,
            SigmaIsle,
            FoldTypFamily,
            LambdaTypFamilyIsle
          )(obj("fiber-node").str)
        }
        (rv("input"), rv("output")) match {
          case (input: RandomVar[u], output: RandomVar[v]) =>
            FlatMap[u, v](input, fiberNode.asInstanceOf[u => GeneratorNode[v]], output)
        }
    }
  }
}
