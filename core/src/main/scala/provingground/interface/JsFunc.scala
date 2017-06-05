package provingground.interface

import provingground._

import translation._

import scala.language.higherKinds

import upickle.Js

import cats._

import cats.implicits._

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
    def encode(t: Int) = Js.Num(t)

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
        (
          xJs.decode(js("first")),
          yJs.decode(js("second"))
        )
    }

  import Translator._

  def toJs[I, F[_]](pat: Pattern[I, F])(name: String,
                                        header: String = "intro")(
      implicit jsF: JsFunc[F]): Translator[I, Js.Value] =
    pat >>> { (js) =>
      Js.Obj(header -> Js.Str(name), "tree" -> jsF.encode(js))
    }

  def jsToOpt[I, F[_]: Traverse](name: String, header: String = "intro")(
      build: F[I] => Option[I])(
      implicit jsF: JsFunc[F]): Translator[Js.Value, I] = {
    val pat = Pattern[Js.Value, F] { (js) =>
      if (js(header) == name) Some(jsF.decode(js("tree"))) else None
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

  val termToJson =
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
      toJs(indInducFunc)("indexed-inductive-function") ||
      toJs(indRecFunc)("indexed-recursive-function") ||
      toJs(recFunc)("recursive-function") ||
      toJs(inducFunc)("inductive-function") ||
      toJs(symbolic)("symbolic") ||
      toJs(firstIncl)("first-inclusion") ||
      toJs(secondIncl)("second-inclusion") ||
      toJs(identityTyp)("equality")

  val jsonToTermBase =
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
      } ||
      jsToBuild[Term, III]("sigma") {
        case ((variable, typ), value: Typ[u]) => sigma(variable)(value)
      } ||
      jsToBuild[Term, II]("product-type") {
        case (x: Typ[u], y: Typ[v]) => ProdTyp(x, y)
      } ||
      jsToBuild[Term, II]("plus-type") {
        case (x: Typ[u], y: Typ[v]) => PlusTyp(x, y)
      } ||
      jsToBuild[Term, II]("pair") { case (x, y) => mkPair(x, y) } ||
      jsToBuild[Term, II]("func-type") {
        case (x: Typ[u], y: Typ[v]) => FuncTyp(x, y)
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
      jsToBuild[Term, II]("first-inclusion") {
        case (tp: PlusTyp[u, v], x) => tp.incl1(x.asInstanceOf[u])
      } ||
      jsToBuild[Term, II]("second-inclusion") {
        case (tp: PlusTyp[u, v], x) => tp.incl2(x.asInstanceOf[v])
      } ||
      jsToOpt[Term, IIV]("recursive-function") {
        case (a, (b, v)) =>
          val fn = buildRecDef()
          fn(a, (b, v))
      } ||
      jsToOpt[Term, IIV]("inductive-function") {
        case (a, (b, v)) =>
          val fn = buildIndDef()
          fn(a, (b, v))
      } ||
      jsToOpt[Term, VIIV]("indexed-recursive-function") {
        case (w, (a, (b, v))) =>
          val fn = buildIndRecDef()
          fn(w, (a, (b, v)))
      } ||
      jsToOpt[Term, VIIV]("indexed-inductive-function") {
        case (w, (a, (b, v))) =>
          val fn = buildIndIndDef()
          fn(w, (a, (b, v)))
      } ||
      jsToBuild[Term, Named]("symbolic") {
        case (name, tp: Typ[u]) => name :: tp
      }(travNamed, implicitly[JsFunc[Named]])

}
