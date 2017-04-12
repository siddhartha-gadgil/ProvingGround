package provingground

import scala.language.higherKinds

import upickle.Js

import cats._

trait JsFunc[F[_]]{
  def encode(t: F[Js.Value]): Js.Value

  def decode(js: Js.Value): F[Js.Value]
}

object JsFunc{
  implicit val idJS : JsFunc[Id] = new JsFunc[Id]{
    def encode(t: Js.Value) = t

    def decode(js: Js.Value) = js
  }

  import Functors.N

  implicit val intJs : JsFunc[N] = new JsFunc[N]{
    def encode(t : Int) = Js.Num(t)

    def decode(js: Js.Value) = js.num.toInt
  }

  implicit val vecJs : JsFunc[Vector] = new JsFunc[Vector]{
    def encode(t: Vector[Js.Value]) = Js.Arr(t : _*)

    def decode(js: Js.Value) = js.arr.toVector
  }

  implicit def pairJS[X[_], Y[_]](implicit xJs: JsFunc[X], yJs: JsFunc[Y]) : JsFunc[({ type Z[A] = (X[A], Y[A]) })#Z] =
    new JsFunc[({ type Z[A] = (X[A], Y[A]) })#Z]{
      def encode(t: (X[Js.Value], Y[Js.Value])) =
        Js.Obj(
          "first" -> xJs.encode(t._1),
          "second" -> yJs.encode(t._2))

      def decode(js: Js.Value) =
        (
          xJs.decode(js("first")),
          yJs.decode(js("second"))
        )
    }

    import Translator._

    def toJs[I, F[_]](pat: Pattern[I, F])(name: String, header: String = "intro")(implicit jsF: JsFunc[F]): Translator[I, Js.Value] =
      pat >>> {(js) =>
        Js.Obj(header -> Js.Str(name), "tree" -> jsF.encode(js))
      }

    def jsToOpt[I, F[_] : Traverse](name: String, header: String = "intro")(build: F[I] => Option[I])(implicit jsF: JsFunc[F]) : Translator[Js.Value, I] = {
      val pat = Pattern[Js.Value, F]{(js) =>
        if (js(header) == name) Some(jsF.decode(js("tree"))) else None
      }
      pat >> build
    }

    def jsToBuild[I, F[_] : Traverse](name: String, header: String = "intro")(build: F[I] => I)(implicit jsF: JsFunc[F]) : Translator[Js.Value, I] = {
      val pat = Pattern[Js.Value, F]{(js) =>
        if (js(header) == name) Some(jsF.decode(js("tree"))) else None
      }
      pat >>> build
    }



}
