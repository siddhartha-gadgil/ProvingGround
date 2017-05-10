package provingground.library

import provingground._

import HoTT._

import TLImplicits._

import shapeless._

object Vecs {
  import Nats._

  val A = "A" :: Type

  val a = "a" :: A

  val Vec = "Vec" :: Nat ->: Type

  val VecInd =
    ("nil" ::: (Vec -> Vec(zero))) |: {
      "cons" ::: n ~>>: (A ->>: (Vec :> Vec(n)) -->>: (Vec -> Vec(succ(n))))
    } =:: Vec

  val vnil :: vcons :: HNil = VecInd.intros

  val vn    = "v_n" :: Vec(n)
  val vm    = "v_m" :: Vec(m)
  val recVN = VecInd.rec(Nat)

  val size = recVN(zero)(n :~> (a :-> (vn :-> (m :-> (succ(m))))))

  val indVVV =
    VecInd.induc(n :~> (vn :-> (m ~>: (Vec(m) ->: Vec(add(n)(m))))))
  val concatVn = "concat(v_n)" :: (m ~>: (Vec(m) ->: Vec(add(n)(m))))
  val vconcat = indVVV(m :~> (vm :-> vm))(
    n :~>
      (a :->
        (vn :->
          (concatVn :->
            (m :~> (vm :-> vcons(add(n)(m))(a)(concatVn(m)(vm))))))))
}
