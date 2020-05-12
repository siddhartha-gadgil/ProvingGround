package provingground.interface

import provingground.{Context, _}
import translation._
import Translator.unmatched

import scala.language.higherKinds
import ujson.Js
import upickle.default._
import cats._
import cats.implicits._
import provingground.induction.{ExstInducDefn, ExstInducStrucs}
import provingground.scalahott.NatRing

import scala.util.matching.Regex
import scala.util._

trait JsFunc[F[_]] {
  def encode(t: F[ujson.Value]): ujson.Value

  def decode(js: ujson.Value): F[ujson.Value]
}

object JsFunc {
  implicit val idJS: JsFunc[Id] = new JsFunc[Id] {
    def encode(t: ujson.Value): ujson.Value = t

    def decode(js: ujson.Value): ujson.Value = js
  }

  import Functors._

  implicit val intJs: JsFunc[N] = new JsFunc[N] {
    def encode(t: Int) = ujson.Num(t.toDouble)

    def decode(js: ujson.Value): Int = js.num.toInt
  }

  implicit val strJs: JsFunc[S] = new JsFunc[S] {
    def encode(t: String) = ujson.Str(t)

    def decode(js: ujson.Value): String = js.str
  }

  implicit val unitJs: JsFunc[Un] = new JsFunc[Un] {
    def encode(t: Unit): ujson.Null.type = ujson.Null

    def decode(js: ujson.Value): Unit = ()
  }

  implicit val vecJs: JsFunc[Vector] = new JsFunc[Vector] {
    def encode(t: Vector[ujson.Value]) = ujson.Arr(t: _*)

    def decode(js: ujson.Value): Vector[ujson.Value] = js.arr.toVector
  }

  implicit def pairJS[X[_], Y[_]](
      implicit xJs: JsFunc[X],
      yJs: JsFunc[Y]
  ): JsFunc[({ type Z[A] = (X[A], Y[A]) })#Z] =
    new JsFunc[({ type Z[A] = (X[A], Y[A]) })#Z] {
      def encode(t: (X[ujson.Value], Y[ujson.Value])) =
        ujson.Obj("first" -> xJs.encode(t._1), "second" -> yJs.encode(t._2))

      def decode(js: ujson.Value): (X[ujson.Value], Y[ujson.Value]) =
        (xJs.decode(js("first")), yJs.decode(js("second")))
    }

  import Translator._

  def toJs[I, F[_]](pat: Pattern[I, F])(name: String, header: String = "intro")(
      implicit jsF: JsFunc[F]
  ): Translator[I, ujson.Value] =
    pat >>> { (js) =>
      ujson.Obj(header -> ujson.Str(name), "tree" -> jsF.encode(js))
    }

  def jsToOpt[I, F[_]: Traverse](name: String, header: String = "intro")(
      build: F[I] => Option[I]
  )(implicit jsF: JsFunc[F]): Translator[ujson.Value, I] = {
    val pat = Pattern[ujson.Value, F] { (js) =>
      if (js(header) == ujson.Str(name)) Some(jsF.decode(js("tree"))) else None
    }
    pat >> build
  }

  def jsToBuild[I, F[_]: Traverse](name: String, header: String = "intro")(
      build: F[I] => I
  )(implicit jsF: JsFunc[F]): Translator[ujson.Value, I] = {
    val pat = Pattern[ujson.Value, F] { (js) =>
      if (js(header) == ujson.Str(name)) Some(jsF.decode(js("tree"))) else None
    }
    pat >>> build
  }

}

import TermPatterns._

import HoTT._

import Functors._

object TermJson {
  import JsFunc._

  implicit val travNamed: Traverse[Named] = traversePair[S, Id]

  val termToJson: Translator.OrElse[Term, ujson.Value] =
    toJs(universe)("universe") ||
      toJs(formalAppln)("appln") ||
      toJs(lambdaTriple)("lambda") ||
      toJs(sigmaTriple)("sigma") ||
      toJs(piTriple)("pi") ||
      toJs(prodTyp)("product-type") ||
      toJs(absPair)("pair") ||
      toJs(plusTyp)("plus-type") ||
      toJs(funcTyp)("func-type") ||
      toJs(star)("star") ||
      toJs(unit)("unit-type") ||
      toJs(zero)("zero-type") ||
      toJs(prop)("prop-universe") ||
      toJs(introIndInducFunc)("intro-indexed-induc-func") ||
      toJs(introIndRecFunc)("intro-indexed-rec-func") ||
      toJs(introInducFunc)("intro-induc-func") ||
      toJs(introRecFunc)("intro-rec-func") ||
      toJs(indInducFunc)("indexed-inductive-function") ||
      toJs(indRecFunc)("indexed-recursive-function") ||
      toJs(recFunc)("recursive-function") ||
      toJs(inducFunc)("inductive-function") ||
      toJs(hashSymbolic)("symbolic") ||
      toJs(mereWitness)("witness") ||
      toJs(firstIncl)("first-inclusion") ||
      toJs(secondIncl)("second-inclusion") ||
      toJs(identityTyp)("equality") ||
      toJs(refl)("reflexivity") ||
      toJs(natTyp)("nat-type") ||
      toJs(natUniv)("nat-univ") ||
      toJs(natZero)("nat-zero") ||
      toJs(natSucc)("nat-succ") ||
      toJs(natSum)("nat-sum") ||
      toJs(natProd)("nat-prod") ||
      toJs(natLiteral)("nat-literal") ||
      toJs(natAddMorph)("nat-additive-morphism") ||
      toJs(foldedTerm)("folded-term") ||
      toJs(miscAppln)("appln")

  def termToJsonGet(t: Term) =
    termToJson(t).getOrElse(throw new Exception(s"cannot serialize term $t"))

  def fdJson(fd: FiniteDistribution[Term]): ujson.Arr = {
    val pmf = for {
      Weighted(elem, p) <- fd.pmf
      tjs               <- termToJson(elem)
    } yield ujson.Obj("term" -> tjs, "weight" -> ujson.Num(p))
    ujson.Arr(pmf: _*)
  }

  import induction._
  import TermLang.applyAll

  val exstInduc = ExstInducStrucs.Base || NatRing

  import library._, Nats._, Bools._, Vecs._

  def jsonToTerm(
      inds: Typ[Term] => Option[ConstructorSeqTL[_, Term, _]] = (_) => None,
      indexedInds: Term => Option[IndexedConstructorSeqDom[_, Term, _, _, _]] =
        (_) => None
  ): Translator.OrElse[ujson.Value, Term] =
    jsonToTermBase ||
      jsToOpt[Term, VIVIIV]("intro-indexed-rec-func") {
        case (intros, (u, (w, (x, (y, v))))) =>
          (buildIndRecDef()(w, (x, (y, v))))
            .orElse(applyAll(ExstInducStrucs.getIndexed(x, w).recOpt(x, toTyp(y)), v))
      } ||
      jsToOpt[Term, VIVIIV]("intro-indexed-induc-func") {
        case (intros, (u, (w, (x, (y, v))))) =>
          (buildIndIndDef()(w, (x, (y, v))))
            .orElse(applyAll(ExstInducStrucs.getIndexed(x, w).inducOpt(x, y), v))
      } ||
      jsToOpt[Term, VIIV]("intro-rec-func") {
        case (w, (x, (y, v))) =>
          (buildRecDef()(x, (y, v)))
            .orElse(applyAll(exstInduc.recOpt(x, toTyp(y)), v))
            .orElse(applyAll(ExstInducStrucs.get(x, w).recOpt(x, toTyp(y)), v))
      } ||
      jsToOpt[Term, VIIV]("intro-induc-func") {
        case (w, (x, (y, v))) =>
          (buildIndDef()(x, (y, v)))
            .orElse {
              val func = exstInduc.inducOpt(x, y)
              applyAll(func, v)
            }
            .orElse {
              val func = ExstInducStrucs.get(x, w).inducOpt(x, y)
              applyAll(func, v)
            }
      } ||
      jsToOpt[Term, IIV]("recursive-function") {
        case (x, (y, v)) =>
          buildRecDef(inds)(x, (y, v))
      } ||
      jsToOpt[Term, IIV]("inductive-function") {
        case (x, (y, v)) => buildIndDef(inds)(x, (y, v))
      } ||
      jsToOpt[Term, IVIIV]("indexed-recursive-function") {
        case (u, (w, (x, (y, v)))) =>
          buildIndRecDef(indexedInds)(w, (x, (y, v)))
      } ||
      jsToOpt[Term, IVIIV]("indexed-inductive-function") {
        case (u, (w, (x, (y, v)))) =>
          buildIndIndDef(indexedInds)(w, (x, (y, v)))
      }

  implicit val termRW : ReadWriter[Term] = readwriter[ujson.Value].bimap(
    term => termToJsonGet(term),
    js => jsonToTerm()(js).get
  )

  def jsToTermExst(
      exst: ExstInducStrucs
  ): Translator.OrElse[ujson.Value, Term] =
    jsonToTermBase ||
      jsToOpt[Term, IIV]("recursive-function") {
        case (dom: Term, (cod: Term, data: Vector[Term])) =>
          for {
            codom <- typOpt(cod)
            fn    <- exst.recOpt(dom, codom)
          } yield data.foldLeft(fn)(fold(_)(_))
        //buildRecDef(inds)(x, (y, v))
      } ||
      jsToOpt[Term, IIV]("inductive-function") {
        case (dom: Term, (cod: Term, data: Vector[Term])) =>
          for {
            fn <- exst.inducOpt(dom, cod)
          } yield data.foldLeft(fn)(fold(_)(_))
      } ||
      jsToOpt[Term, IVIIV]("indexed-recursive-function") {
        case (
            _,
            (index: Vector[Term], (dom: Term, (cod: Term, data: Vector[Term])))
            ) =>
          for {
            codom <- typOpt(cod)
            fn    <- exst.recOpt(dom, codom)
          } yield
            (data ++ index).foldLeft(fn)(fold(_)(_)) //buildIndRecDef(indexedInds)(w, (x, (y, v)))
      } ||
      jsToOpt[Term, IVIIV]("indexed-inductive-function") {
        case (
            _,
            (index: Vector[Term], (dom: Term, (cod: Term, data: Vector[Term])))
            ) =>
          for {
            fn <- exst.inducOpt(dom, cod)
          } yield (data ++ index).foldLeft(fn)(fold(_)(_))
        //buildIndIndDef(indexedInds)(w, (x, (y, v)))
      }

  def jsToFD(
      exst: ExstInducStrucs
  )(js: ujson.Value): FiniteDistribution[Term] = {
    val pmf =
      js.arr.toVector.map { wp =>
        Weighted(
          jsToTermExst(exst)(wp.obj("term")).get,
          wp.obj("weight").num
        )
      }
    FiniteDistribution(pmf)
  }

  val jsonToTermBase: Translator.OrElse[ujson.Value, Term] =
    jsToBuild[Term, N]("universe")((n) => Universe(n)) ||
      jsToBuild[Term, II]("appln") { case (func, arg) => fold(func)(arg) } ||
      jsToBuild[Term, III]("lambda") {
        case ((variable, typ), value) => variable :~> value
      } ||
      jsToBuild[Term, III]("equality") {
        case ((dom, lhs), rhs) => lhs =:= rhs
      } ||
      jsToBuild[Term, III]("pi") {
        case ((variable, typ), value: Typ[u]) => variable ~>: value
        case (x, y)                           => unmatched(x, y)
      } ||
      jsToBuild[Term, III]("sigma") {
        case ((variable, typ), value: Typ[u]) => sigma(variable)(value)
        case (x, y)                           => unmatched(x, y)
      } ||
      jsToBuild[Term, II]("product-type") {
        case (x: Typ[u], y: Typ[v]) => ProdTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      jsToBuild[Term, II]("plus-type") {
        case (x: Typ[u], y: Typ[v]) => PlusTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      jsToBuild[Term, II]("pair") { case (x, y) => mkPair(x, y) } ||
      jsToBuild[Term, II]("func-type") {
        case (x: Typ[u], y: Typ[v]) => FuncTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      jsToBuild[Term, II]("reflexivity") {
        case (dom: Typ[u], value: Term) => Refl(dom, value)
        case (x, y)                     => unmatched(x, y)
      } ||
      jsToBuild[Term, IV]("folded-term") {
        case (op, v) =>
          v.reduce[Term] {
            case (a: Term, b: Term) => applyFunc(applyFunc(op, a), b)
          }
      } ||
      jsToBuild[Term, Un]("star") { (_) =>
        Star
      } ||
      jsToBuild[Term, Un]("unit-type") { (_) =>
        Unit
      } ||
      jsToBuild[Term, Un]("zero-type") { (_) =>
        Zero
      } ||
      jsToBuild[Term, Un]("prop-universe") { (_) =>
        Prop
      } ||
      jsToBuild[Term, Un]("nat-type") { (_) =>
        NatRing.NatTyp
      } ||
      jsToBuild[Term, Un]("nat-univ") { (_) =>
        NatRing.NatTyp.typ
      } ||
      jsToBuild[Term, Un]("nat-zero") { (_) =>
        NatRing.zero
      } ||
      jsToBuild[Term, Un]("nat-succ") { (_) =>
        NatRing.succ
      } ||
      jsToBuild[Term, Un]("nat-sum") { (_) =>
        NatRing.sum
      } ||
      jsToBuild[Term, Un]("nat-prod") { (_) =>
        NatRing.prod
      } ||
      jsToBuild[Term, N]("nat-literal") { (n) =>
        NatRing.Literal(n)
      } ||
      jsToBuild[Term, II]("nat-additive-morphism") {
        case (base, op) =>
          NatRing.AdditiveMorphism(
            base.asInstanceOf[Func[NatRing.Nat, NatRing.Nat]],
            op.asInstanceOf[(NatRing.Nat, NatRing.Nat) => NatRing.Nat]
          )

      } ||
      jsToBuild[Term, II]("first-inclusion") {
        case (tp: PlusTyp[u, v], x) => tp.incl1(x.asInstanceOf[u])
        case (x, y)                 => unmatched(x, y)
      } ||
      jsToBuild[Term, II]("second-inclusion") {
        case (tp: PlusTyp[u, v], x) => tp.incl2(x.asInstanceOf[v])
        case (x, y)                 => unmatched(x, y)
      } ||
      jsToOpt[Term, IIV]("recursive-function") {
        case (a, (b, v)) =>
          // println(s"building base recursive type $a codomain $b data $v")
          val fn = buildRecDef()
          fn(a, (b, v))
      } ||
      jsToOpt[Term, IIV]("inductive-function") {
        case (a, (b, v)) =>
          val fn = buildIndDef()
          fn(a, (b, v))
      } ||
      jsToOpt[Term, IVIIV]("indexed-recursive-function") {
        case (u, (w, (a, (b, v)))) =>
          // println(s"building indexed recursive:\n index $w,\n type $a,\n codomain $b,\n data $v\n\n")
          val fn  = buildIndRecDef()
          val res = fn(w, (a, (b, v)))
          println(s"result: $res")
          res
      } ||
      jsToOpt[Term, IVIIV]("indexed-inductive-function") {
        case (u, (w, (a, (b, v)))) =>
          // println(s"building indexed inductive:\n index $w,\n type $a,\n codomain $b,\n data $v\n\n")
          val fn  = buildIndIndDef()
          val res = fn(w, (a, (b, v)))
          // println(s"result: $res")
          res
      } ||
      jsToBuild[Term, Named]("symbolic") {
        case (name, tp: Typ[u]) => deHash(name) :: tp
        case (x, y)             => unmatched(x, y)
      }(travNamed, implicitly[JsFunc[Named]]) ||
      jsToBuild[Term, II]("Witness") {
        case (tp, value) => toTyp(tp).symbObj(MereWitness(value))
      }

  val hashReg: Regex = "_[0-9][0-9]+".r

  def deHash(s: String): String = hashReg.replaceAllIn(s, "")

}

object InducJson {
  import TermJson._, ExstInducStrucs._

  def toJson(exst: ExstInducStrucs): ujson.Value = exst match {
    case Base    => ujson.Obj("intro" -> "base")
    case NatRing => ujson.Obj("intro" -> "nat-ring")
    case OrElse(first, second) =>
      ujson.Obj(
        "intro"  -> "or-else",
        "first"  -> toJson(first),
        "second" -> toJson(second)
      )
    case LambdaInduc(x, struc) =>
      ujson.Obj(
        "intro"     -> "lambda",
        "variable"  -> termToJsonGet(x),
        "structure" -> toJson(struc)
      )
    case ConsSeqExst(cs, intros) =>
      ujson.Obj(
        "intro" -> "constructor-sequence",
        "type"  -> termToJsonGet(cs.typ),
        "intros" -> ujson.Arr(intros.map { (t) =>
          termToJsonGet(t)
        }: _*)
      )
    case ind @ IndConsSeqExst(cs, intros) =>
      ujson.Obj(
        "intro" -> "indexed-constructor-sequence",
        "type"  -> termToJsonGet(ind.fmly),
        "intros" -> ujson.Arr(intros.map { (t) =>
          termToJsonGet(t)
        }: _*)
      )
  }

  def fdJson(fd: FiniteDistribution[ExstInducDefn]): ujson.Arr = {
    val pmf = for {
      Weighted(elem, p) <- fd.pmf
    } yield
      ujson.Obj(
        "type-family" -> termToJsonGet(elem.typFamily),
        "introduction-rules" -> ujson.Arr(
          (elem.intros.map((t) => termToJsonGet(t))): _*
        ),
        "structure" -> toJson(elem.ind),
        "weight"    -> ujson.Num(p)
      )
    ujson.Arr(pmf: _*)
  }

  def fromJson(init: ExstInducStrucs)(js: ujson.Value): ExstInducStrucs =
    js.obj("intro").str match {
      case "base"     => Base
      case "nat-ring" => NatRing
      case "or-else" =>
        OrElse(
          fromJson(init)(js.obj("first")),
          fromJson(init)(js.obj("second"))
        )
      case "lambda" =>
        val x     = jsToTermExst(init)(js.obj("variable")).get
        val struc = fromJson(init)(js.obj("structure"))
        LambdaInduc(x, struc)
      case "constructor-sequence" =>
        val typ = jsToTermExst(init)(js.obj("type")).flatMap(typOpt).get
        val intros =
          js.obj("intros").arr.map((t) => jsToTermExst(init)(t).get).toVector
        get(typ, intros)
      case "indexed-constructor-sequence" =>
        val typF = jsToTermExst(init)(js.obj("type")).get
        val intros =
          js.obj("intros").arr.map((t) => jsToTermExst(init)(t).get).toVector
        getIndexed(typF, intros)
    }

  def jsToFD(
      exst: ExstInducStrucs
  )(js: ujson.Value): FiniteDistribution[ExstInducDefn] = {
    val pmf =
      js.arr.toVector.map { wp =>
        val ind       = fromJson(exst)(wp.obj("structure"))
        val typFamily = jsToTermExst(exst)(wp.obj("type-family")).get
        val intros = wp
          .obj("introduction-rules")
          .arr
          .toVector
          .map((t) => jsToTermExst(exst)(t).get)
        val parameters = wp
          .obj("parameters")
          .arr
          .toVector
          .map((t) => jsToTermExst(exst)(t).get)
        Weighted(
          ExstInducDefn(typFamily, intros, ind),
          wp.obj("weight").num
        )
      }
    FiniteDistribution(pmf)
  }

}

object ContextJson {
  import Context._, TermJson._
  def toJson(ctx: Context): ujson.Value = ctx match {
    case Empty => ujson.Obj("intro" -> "empty")
    case ac: AppendConstant[u] =>
      ujson.Obj(
        "intro"    -> "append-constant",
        "init"     -> toJson(ac.init),
        "constant" -> termToJsonGet(ac.constant)
      )
    case at: AppendTerm[u] =>
      val rl = at.role match {
        case Context.Assert   => ujson.Str("assert")
        case Context.Consider => ujson.Str("consider")
      }
      ujson.Obj(
        "intro" -> "append-term",
        "init"  -> toJson(at.init),
        "term"  -> termToJsonGet(at.term),
        "role"  -> rl
      )
    case av: AppendVariable[u] =>
      ujson.Obj(
        "intro"      -> "append-variable",
        "init"       -> toJson(av.init),
        "expression" -> termToJsonGet(av.variable)
      )
    case AppendDefn(init, defn, global) =>
      ujson.Obj(
        "intro"  -> "append-definition",
        "name"   -> termToJsonGet(defn.name),
        "init"   -> toJson(init),
        "value"  -> termToJsonGet(defn.valueTerm),
        "global" -> ujson.Bool(global)
      )
    case AppendIndDef(init, defn) =>
      ujson.Obj(
        "intro" -> "append-inductive-definition",
        "defn"  -> InducJson.toJson(defn),
        "init"  -> toJson(init)
      )
  }

  def fromJson(js: ujson.Value): Context =
    js.obj("intro").str match {
      case "empty" => Empty
      case "append-term" =>
        val init = fromJson(js.obj("init"))
        val term = jsToTermExst(init.inducStruct)(js.obj("term")).get
        val role = js.obj("role").str match {
          case "assert"   => Assert
          case "consider" => Consider
        }
        AppendTerm(init, term, role)
      case "append-constant" =>
        val init = fromJson(js.obj("init"))
        val term = jsToTermExst(init.inducStruct)(js.obj("constant")).get
        AppendConstant(init, term)
      case "append-variable" =>
        val init = fromJson(js.obj("init"))
        val term = jsToTermExst(init.inducStruct)(js.obj("variable")).get
        AppendVariable(init, term)
      case "append-definition" =>
        val init  = fromJson(js.obj("init"))
        val name  = jsToTermExst(init.inducStruct)(js.obj("name")).get
        val value = jsToTermExst(init.inducStruct)(js.obj("value")).get
        AppendDefn(init, Defn(name, value), js.obj("global").bool)
      case "append-inductive-definition" =>
        val init = fromJson(js.obj("init"))
        val defn = InducJson.fromJson(init.inducStruct)(js.obj("defn"))
        AppendIndDef(init, defn)
    }
}

object ConciseTermJson {
  import JsFunc._

  implicit val travNamed: Traverse[Named] = traversePair[S, Id]

  val termToJson: Translator.OrElse[Term, ujson.Value] =
    toJs(universe)("U") ||
      toJs(formalAppln)("Ap") ||
      toJs(lambdaTriple)("\u03bb") ||
      toJs(sigmaTriple)("\u03c3") ||
      toJs(piTriple)("\u03c0") ||
      toJs(prodTyp)("\u03a0") ||
      toJs(absPair)("pair") ||
      toJs(plusTyp)("\u03a3") ||
      toJs(funcTyp)(UnicodeSyms.Arrow) ||
      toJs(star)("*") ||
      toJs(unit)("One") ||
      toJs(zero)("Void") ||
      toJs(prop)("Prop") ||
      toJs(indInducFunc)("IInd") ||
      toJs(indRecFunc)("IRec") ||
      toJs(recFunc)("Rec") ||
      toJs(inducFunc)("Ind") ||
      toJs(hashSymbolic)("Sym") ||
      toJs(mereWitness)("Witness") ||
      toJs(firstIncl)("i1") ||
      toJs(secondIncl)("i2") ||
      toJs(identityTyp)("=") ||
      toJs(refl)("=") ||
      toJs(natTyp)("Nat") ||
      toJs(natUniv)("NatU") ||
      toJs(natZero)("0N") ||
      toJs(natSucc)("succN") ||
      toJs(natSum)("+N") ||
      toJs(natProd)("*N") ||
      toJs(natLiteral)("NL") ||
      toJs(natAddMorph)("nat-additive-morphism") ||
      toJs(foldedTerm)("folded-term") ||
      toJs(miscAppln)("Ap")

  def termToJsonGet(t: Term): ujson.Value =
    termToJson(t).getOrElse(throw new Exception(s"cannot serialize term $t"))

  def fdJson(fd: FiniteDistribution[Term]): ujson.Arr = {
    val pmf = for {
      Weighted(elem, p) <- fd.pmf
      tjs               <- termToJson(elem)
    } yield ujson.Obj("term" -> tjs, "weight" -> ujson.Num(p))
    ujson.Arr(pmf: _*)
  }

  import induction._

  def jsonToTerm(
      inds: Typ[Term] => Option[ConstructorSeqTL[_, Term, _]] = (_) => None,
      indexedInds: Term => Option[IndexedConstructorSeqDom[_, Term, _, _, _]] =
        (_) => None
  ): Translator.OrElse[ujson.Value, Term] =
    jsonToTermBase ||
      jsToOpt[Term, IIV]("recursive-function") {
        case (x, (y, v)) =>
          buildRecDef(inds)(x, (y, v))
      } ||
      jsToOpt[Term, IIV]("inductive-function") {
        case (x, (y, v)) => buildIndDef(inds)(x, (y, v))
      } ||
      jsToOpt[Term, IVIIV]("indexed-recursive-function") {
        case (u, (w, (x, (y, v)))) =>
          buildIndRecDef(indexedInds)(w, (x, (y, v)))
      } ||
      jsToOpt[Term, IVIIV]("indexed-inductive-function") {
        case (u, (w, (x, (y, v)))) =>
          buildIndIndDef(indexedInds)(w, (x, (y, v)))
      }

  def jsToTermExst(
      exst: ExstInducStrucs
  ): Translator.OrElse[ujson.Value, Term] =
    jsonToTermBase ||
      jsToOpt[Term, IIV]("recursive-function") {
        case (dom: Term, (cod: Term, data: Vector[Term])) =>
          for {
            codom <- typOpt(cod)
            fn    <- exst.recOpt(dom, codom)
          } yield data.foldLeft(fn)(fold(_)(_))
        //buildRecDef(inds)(x, (y, v))
      } ||
      jsToOpt[Term, IIV]("inductive-function") {
        case (dom: Term, (cod: Term, data: Vector[Term])) =>
          for {
            fn <- exst.inducOpt(dom, cod)
          } yield data.foldLeft(fn)(fold(_)(_))
      } ||
      jsToOpt[Term, IVIIV]("indexed-recursive-function") {
        case (
            _,
            (index: Vector[Term], (dom: Term, (cod: Term, data: Vector[Term])))
            ) =>
          for {
            codom <- typOpt(cod)
            fn    <- exst.recOpt(dom, codom)
          } yield
            (data ++ index).foldLeft(fn)(fold(_)(_)) //buildIndRecDef(indexedInds)(w, (x, (y, v)))
      } ||
      jsToOpt[Term, IVIIV]("indexed-inductive-function") {
        case (
            _,
            (index: Vector[Term], (dom: Term, (cod: Term, data: Vector[Term])))
            ) =>
          for {
            fn <- exst.inducOpt(dom, cod)
          } yield (data ++ index).foldLeft(fn)(fold(_)(_))
        //buildIndIndDef(indexedInds)(w, (x, (y, v)))
      }

  def jsToFD(
      exst: ExstInducStrucs
  )(js: ujson.Value): FiniteDistribution[Term] = {
    val pmf =
      js.arr.toVector.map { wp =>
        Weighted(
          jsToTermExst(exst)(wp.obj("term")).get,
          wp.obj("weight").num
        )
      }
    FiniteDistribution(pmf)
  }

  val jsonToTermBase: Translator.OrElse[ujson.Value, Term] =
    jsToBuild[Term, N]("U")((n) => Universe(n)) ||
      jsToBuild[Term, II]("Ap") { case (func, arg) => fold(func)(arg) } ||
      jsToBuild[Term, III]("\u03bb") {
        case ((variable, typ), value) => variable :~> value
      } ||
      jsToBuild[Term, III]("=") {
        case ((dom, lhs), rhs) => lhs =:= rhs
      } ||
      jsToBuild[Term, III]("\u03c0") {
        case ((variable, typ), value: Typ[u]) => variable ~>: value
        case (x, y)                           => unmatched(x, y)
      } ||
      jsToBuild[Term, III]("\u03c3") {
        case ((variable, typ), value: Typ[u]) => sigma(variable)(value)
        case (x, y)                           => unmatched(x, y)
      } ||
      jsToBuild[Term, II]("\u03a0") {
        case (x: Typ[u], y: Typ[v]) => ProdTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      jsToBuild[Term, II]("\u03a3") {
        case (x: Typ[u], y: Typ[v]) => PlusTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      jsToBuild[Term, II]("pair") { case (x, y) => mkPair(x, y) } ||
      jsToBuild[Term, II](UnicodeSyms.Arrow) {
        case (x: Typ[u], y: Typ[v]) => FuncTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      jsToBuild[Term, II]("=") {
        case (dom: Typ[u], value: Term) => Refl(dom, value)
        case (x, y)                     => unmatched(x, y)
      } ||
      jsToBuild[Term, IV]("folded-term") {
        case (op, v) =>
          v.reduce[Term] {
            case (a: Term, b: Term) => applyFunc(applyFunc(op, a), b)
          }
      } ||
      jsToBuild[Term, Un]("*") { (_) =>
        Star
      } ||
      jsToBuild[Term, Un]("One") { (_) =>
        Unit
      } ||
      jsToBuild[Term, Un]("Void") { (_) =>
        Zero
      } ||
      jsToBuild[Term, Un]("Prop") { (_) =>
        Prop
      } ||
      jsToBuild[Term, Un]("Nat") { (_) =>
        NatRing.NatTyp
      } ||
      jsToBuild[Term, Un]("NatU") { (_) =>
        NatRing.NatTyp.typ
      } ||
      jsToBuild[Term, Un]("0N") { (_) =>
        NatRing.zero
      } ||
      jsToBuild[Term, Un]("succN") { (_) =>
        NatRing.succ
      } ||
      jsToBuild[Term, Un]("+N") { (_) =>
        NatRing.sum
      } ||
      jsToBuild[Term, Un]("*N") { (_) =>
        NatRing.prod
      } ||
      jsToBuild[Term, N]("NL") { (n) =>
        NatRing.Literal(n)
      } ||
      jsToBuild[Term, II]("nat-additive-morphism") {
        case (base, op) =>
          NatRing.AdditiveMorphism(
            base.asInstanceOf[Func[NatRing.Nat, NatRing.Nat]],
            op.asInstanceOf[(NatRing.Nat, NatRing.Nat) => NatRing.Nat]
          )

      } ||
      jsToBuild[Term, II]("i1") {
        case (tp: PlusTyp[u, v], x) => tp.incl1(x.asInstanceOf[u])
        case (x, y)                 => unmatched(x, y)
      } ||
      jsToBuild[Term, II]("i2") {
        case (tp: PlusTyp[u, v], x) => tp.incl2(x.asInstanceOf[v])
        case (x, y)                 => unmatched(x, y)
      } ||
      jsToOpt[Term, IIV]("Rec") {
        case (a, (b, v)) =>
          // println(s"building base recursive type $a codomain $b data $v")
          val fn = buildRecDef()
          fn(a, (b, v))
      } ||
      jsToOpt[Term, IIV]("Ind") {
        case (a, (b, v)) =>
          val fn = buildIndDef()
          fn(a, (b, v))
      } ||
      jsToOpt[Term, IVIIV]("IRec") {
        case (u, (w, (a, (b, v)))) =>
          // println(s"building indexed recursive:\n index $w,\n type $a,\n codomain $b,\n data $v\n\n")
          val fn  = buildIndRecDef()
          val res = fn(w, (a, (b, v)))
          println(s"result: $res")
          res
      } ||
      jsToOpt[Term, IVIIV]("IInd") {
        case (u, (w, (a, (b, v)))) =>
          // println(s"building indexed inductive:\n index $w,\n type $a,\n codomain $b,\n data $v\n\n")
          val fn  = buildIndIndDef()
          val res = fn(w, (a, (b, v)))
          // println(s"result: $res")
          res
      } ||
      jsToBuild[Term, Named]("Sym") {
        case (name, tp: Typ[u]) => deHash(name) :: tp
        case (x, y)             => unmatched(x, y)
      }(travNamed, implicitly[JsFunc[Named]]) ||
      jsToBuild[Term, II]("Witness") {
        case (tp, value) => toTyp(tp).symbObj(MereWitness(value))
      }

  val hashReg: Regex = "_[0-9][0-9]+".r

  def deHash(s: String): String = hashReg.replaceAllIn(s, "")

  object ContextJson {
    import Context._
    def toJson(ctx: Context): ujson.Value = ctx match {
      case Empty => ujson.Obj("intro" -> "empty")
      case ac: AppendConstant[u] =>
        ujson.Obj(
          "intro"    -> "append-constant",
          "init"     -> toJson(ac.init),
          "constant" -> termToJsonGet(ac.constant)
        )
      case at: AppendTerm[u] =>
        val rl = at.role match {
          case Context.Assert   => ujson.Str("assert")
          case Context.Consider => ujson.Str("consider")
        }
        ujson.Obj(
          "intro" -> "append-term",
          "init"  -> toJson(at.init),
          "term"  -> termToJsonGet(at.term),
          "role"  -> rl
        )
      case av: AppendVariable[u] =>
        ujson.Obj(
          "intro"      -> "append-variable",
          "init"       -> toJson(av.init),
          "expression" -> termToJsonGet(av.variable)
        )
      case AppendDefn(init, defn, global) =>
        ujson.Obj(
          "intro"  -> "append-definition",
          "name"   -> termToJsonGet(defn.name),
          "init"   -> toJson(init),
          "value"  -> termToJsonGet(defn.valueTerm),
          "global" -> ujson.Bool(global)
        )
      case AppendIndDef(init, defn) =>
        ujson.Obj(
          "intro" -> "append-inductive-definition",
          "defn"  -> InducJson.toJson(defn),
          "init"  -> toJson(init)
        )
    }

    def fromJson(js: ujson.Value): Context =
      js.obj("intro").str match {
        case "empty" => Empty
        case "append-term" =>
          val init = fromJson(js.obj("init"))
          val term = jsToTermExst(init.inducStruct)(js.obj("term")).get
          val role = js.obj("role").str match {
            case "assert"   => Assert
            case "consider" => Consider
          }
          AppendTerm(init, term, role)
        case "append-constant" =>
          val init = fromJson(js.obj("init"))
          val term = jsToTermExst(init.inducStruct)(js.obj("constant")).get
          AppendConstant(init, term)
        case "append-variable" =>
          val init = fromJson(js.obj("init"))
          val term = jsToTermExst(init.inducStruct)(js.obj("variable")).get
          AppendVariable(init, term)
        case "append-definition" =>
          val init  = fromJson(js.obj("init"))
          val name  = jsToTermExst(init.inducStruct)(js.obj("name")).get
          val value = jsToTermExst(init.inducStruct)(js.obj("value")).get
          AppendDefn(init, Defn(name, value), js.obj("global").bool)
        case "append-inductive-definition" =>
          val init = fromJson(js.obj("init"))
          val defn = InducJson.fromJson(init.inducStruct)(js.obj("defn"))
          AppendIndDef(init, defn)
      }
  }

}
