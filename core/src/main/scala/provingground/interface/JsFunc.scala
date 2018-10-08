package provingground.interface

import provingground._
import translation._
import Translator.unmatched

import scala.language.higherKinds
import upickle.Js
import cats._
import cats.implicits._
import ujson.Js.Value

trait JsFunc[F[_]] {
  def encode(t: F[Js.Value]): Js.Value

  def decode(js: Js.Value): F[Js.Value]
}

object JsFunc {
  implicit val idJS: JsFunc[Id] = new JsFunc[Id] {
    def encode(t: Js.Value) = t

    def decode(js: Js.Value) = js
  }

  import Functors._

  implicit val intJs: JsFunc[N] = new JsFunc[N] {
    def encode(t: Int) = Js.Num(t.toDouble)

    def decode(js: Js.Value) = js.num.toInt
  }

  implicit val strJs: JsFunc[S] = new JsFunc[S] {
    def encode(t: String) = Js.Str(t)

    def decode(js: Js.Value) = js.str
  }

  implicit val unitJs: JsFunc[Un] = new JsFunc[Un] {
    def encode(t: Unit) = Js.Null

    def decode(js: Js.Value) = ()
  }

  implicit val vecJs: JsFunc[Vector] = new JsFunc[Vector] {
    def encode(t: Vector[Js.Value]) = Js.Arr(t: _*)

    def decode(js: Js.Value) = js.arr.toVector
  }

  implicit def pairJS[X[_], Y[_]](
      implicit xJs: JsFunc[X],
      yJs: JsFunc[Y]): JsFunc[({ type Z[A] = (X[A], Y[A]) })#Z] =
    new JsFunc[({ type Z[A] = (X[A], Y[A]) })#Z] {
      def encode(t: (X[Js.Value], Y[Js.Value])) =
        Js.Obj("first" -> xJs.encode(t._1), "second" -> yJs.encode(t._2))

      def decode(js: Js.Value) =
        (xJs.decode(js("first")), yJs.decode(js("second")))
    }

  import Translator._

  def toJs[I, F[_]](pat: Pattern[I, F])(name: String, header: String = "intro")(
      implicit jsF: JsFunc[F]): Translator[I, Js.Value] =
    pat >>> { (js) =>
      Js.Obj(header -> Js.Str(name), "tree" -> jsF.encode(js))
    }

  def jsToOpt[I, F[_]: Traverse](name: String, header: String = "intro")(
      build: F[I] => Option[I])(
      implicit jsF: JsFunc[F]): Translator[Js.Value, I] = {
    val pat = Pattern[Js.Value, F] { (js) =>
      if (js(header) == Js.Str(name)) Some(jsF.decode(js("tree"))) else None
    }
    pat >> build
  }

  def jsToBuild[I, F[_]: Traverse](name: String, header: String = "intro")(
      build: F[I] => I)(implicit jsF: JsFunc[F]): Translator[Js.Value, I] = {
    val pat = Pattern[Js.Value, F] { (js) =>
      if (js(header) == Js.Str(name)) Some(jsF.decode(js("tree"))) else None
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

  val termToJson: Translator.OrElse[Term, Value] =
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
      toJs(indInducFunc)("indexed-inductive-function") ||
      toJs(indRecFunc)("indexed-recursive-function") ||
      toJs(recFunc)("recursive-function") ||
      toJs(inducFunc)("inductive-function") ||
      toJs(hashSymbolic)("symbolic") ||
      toJs(firstIncl)("first-inclusion") ||
      toJs(secondIncl)("second-inclusion") ||
      toJs(identityTyp)("equality") ||
      toJs(refl)("reflexivity")

  import induction._

  def jsonToTerm(
      inds: Typ[Term] => Option[ConstructorSeqTL[_, Term, _]] = (_) => None,
      indexedInds: Term => Option[IndexedConstructorSeqDom[_, Term, _, _, _]] =
        (_) => None): Translator.OrElse[Value, Term] =
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

  def jsToTermExst(exst : ExstInducStrucs): Translator.OrElse[Value, Term] =
    jsonToTermBase ||
      jsToOpt[Term, IIV]("recursive-function") {
        case (dom : Term, (cod: Term, data: Vector[Term])) =>
          for{
            codom <- typOpt(cod)
            fn <- exst.recOpt(dom, codom)
          } yield (fn /: data)(fold(_)(_))
           //buildRecDef(inds)(x, (y, v))
      } ||
      jsToOpt[Term, IIV]("inductive-function") {
        case (dom : Term, (cod: Term, data: Vector[Term])) =>
          for{
            fn <- exst.inducOpt(dom, cod)
          } yield (fn /: data)(fold(_)(_))
      } ||
      jsToOpt[Term, IVIIV]("indexed-recursive-function") {
        case (_, (index: Vector[Term], (dom: Term, (cod: Term, data: Vector[Term])))) =>
          for{
            codom <- typOpt(cod)
            fn <- exst.recOpt(dom, codom)
          } yield (fn /: (data ++ index))(fold(_)(_))//buildIndRecDef(indexedInds)(w, (x, (y, v)))
      } ||
      jsToOpt[Term, IVIIV]("indexed-inductive-function") {
        case (_, (index: Vector[Term], (dom: Term, (cod: Term, data: Vector[Term])))) =>
          for{
            fn <- exst.inducOpt(dom, cod)
          } yield (fn /: (data ++ index))(fold(_)(_))
        //buildIndIndDef(indexedInds)(w, (x, (y, v)))
      }


  val jsonToTermBase: Translator.OrElse[Value, Term] =
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
        case (name, tp: Typ[u]) => name :: tp
        case (x, y)             => unmatched(x, y)
      }(travNamed, implicitly[JsFunc[Named]])

}
