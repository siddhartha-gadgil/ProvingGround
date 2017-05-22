package provingground.library

import provingground._

import HoTT._

import induction.TLImplicits._

import shapeless._

object SimpleEvens {
  import Nats.{Nat => Nt, _}
  val isEven      = "isEven" :: Nt ->: Type
  val zeroEven    = "0even" :: isEven(zero)
  val plusTwoEven = "_+2even" :: (n ~>: (isEven(n) ->: isEven(succ(succ(n)))))
}

object DoubleEven {
  import Nats.{Nat => Nt, _}

  import SimpleEvens._

  val thm = n ~>: isEven(double(n))

  val hyp = "isEven(double(n))" :: isEven(double(n))

  val inductor = NatInd.induc(n :-> isEven(double(n)))

  val pf =
    inductor(zeroEven) {
      n :~> (
        hyp :-> (
          plusTwoEven(double(n))(hyp)
        )
      )
    } !: thm
}

object SuccNOrNEven {
  import SimpleEvens._

  import Nats.{Nat => Nt, _}

  val claim = n :-> (isEven(n) || isEven(succ(n)))

  val base = claim(zero).incl1(zeroEven) !: claim(zero)

  val hyp1 = "n-is-Even" :: isEven(n)

  val hyp2 = "(n+1)-is-Even" :: isEven(succ(n))

  val thm = n ~>: (claim(n))

  val step = n :~> {
    (claim(n).rec(claim(succ(n)))) {
      hyp1 :-> (claim(succ(n)).incl2(plusTwoEven(n)(hyp1)))
    } {
      hyp2 :-> (claim(succ(n)).incl1((hyp2)))
    }
  }

  val inductor = NatInd.induc(claim)

  val pf = inductor(base)(step) !: thm
}

object LocalConstImpliesConst {
  import Nats.{Nat => Nt, _}

  val A = "A" :: Type

  val f = "f" :: Nt ->: A

  val ass = "assumption" :: n ~>: (f(n) =:= f(succ(n)))

  val claim = n :-> (f(zero) =:= f(n))

  val base = f(zero).refl

  val hyp = "hypothesis" :: (f(zero) =:= f(n))
  val step = hyp :-> {
    IdentityTyp.trans(A)(f(zero))(f(n))(f(succ(n)))(hyp)(ass(n))
  }

  val thm = n ~>: (claim(n))

  val inductor = NatInd.induc(claim)

  val pf = inductor(n :~> step) !: thm
}
