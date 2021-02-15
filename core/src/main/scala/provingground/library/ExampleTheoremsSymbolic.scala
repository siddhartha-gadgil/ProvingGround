package provingground.library

import provingground._

import HoTT._, scalahott._

import induction.TLImplicits._

import shapeless._

import NatRing.{NatTyp => Nt, _}

import spire.implicits._

object SimpleEvensSym {
  val n = Nt.sym

  val isEven      = "isEven" :: Nt ->: Type
  val zeroEven    = "0even" :: isEven(Literal(0))
  val plusTwoEven = "_+2even" :: (n ~>: (isEven(n) ->: isEven(succ(succ(n)))))

  val double = n :-> (n + n)
}

object DoubleEvenSym {

  import SimpleEvensSym._

  val thm = n ~>: isEven(double(n))

  val hyp = "isEven(double(n))" :: isEven(double(n))

  val inductor = NatRing.induc(n :-> isEven(double(n)))

  val pf =
    inductor(zeroEven) {
      n :~> (hyp :-> (plusTwoEven(double(n))(hyp)))
    } !: thm
}

object SuccNOrNEvenSym {
  import SimpleEvensSym._

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

  val inductor = NatRing.induc(claim)

  val pf = inductor(base)(step) !: thm
}

object LocalConstImpliesConstSym {

  import SimpleEvensSym._

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

  val inductor = NatRing.induc(claim)

  val pf = inductor(base)(n :~> step) !: thm
}

object EqualityNats {
  import NatRing._
  val n = NatTyp.sym
  val m = NatTyp.sym
  val k = NatTyp.sym

  val A = Type.sym

  val recNU  = NatRing.rec(Type)
  val recNNU = NatRing.rec(NatTyp ->: Type)

  val eqm = "Eq(m)" :: NatTyp ->: Type

  val Eq = recNNU(recNU(One)(n :-> (A :-> (Zero: Typ[Term]))))(
    m :-> (eqm :-> (
      recNU(Zero)(n :-> (A :-> (eqm(n))))
    ))
  )

  val eqkk = "Eq(k)(k)" :: Eq(k)(k)

  val diag = NatRing.induc(k :-> Eq(k)(k))(Star)(k :~> (eqkk :-> eqkk))

  val equalmn = "Eq(m)(n)" :: (m =:= n)

  val eqFamily =
    IdentityTyp.induc(NatTyp, 
    m :~> (n :~> (equalmn :~> (Eq(m)(n))))
    )(diag)

  val zeroNotSucc = n :~> (eqFamily(zero)(succ(n)) !: ((zero =:= succ(n)) ->: Zero))
  
  val succNotZero = n :~> (eqFamily(succ(n))(zero) !: ((succ(n) =:= zero ) ->: Zero))
}
