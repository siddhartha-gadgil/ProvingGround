package provingground.library

import provingground._

import HoTT._

object SimpleEvens {
  import Nats.{Nat => Nt, _}
  val isEven      = "isEven" :: Nt ->: Type
  val zeroEven    = "0even" :: isEven(zero)
  val plusTwoEven = "_+2even" :: (n ~>: (isEven(n) ->: isEven(succ(succ(n)))))
}

object DoubleEven {
  import Nats.{Nat => _, _}

  import SimpleEvens._

  val thm = n ~>: isEven(double(n))

  val hyp = "isEven(double(n))" :: isEven(double(n))

  val inductor = NatInd.induc(n :-> isEven(double(n)))

  val pf =
    inductor(zeroEven) {
      n :~> (hyp :-> (plusTwoEven(double(n))(hyp)))
    } !: thm
}

object SuccNOrNEven {
  import SimpleEvens._

  import Nats.{Nat => _, _}

  val claim: Func[Term, PlusTyp[Term, Term]] = n :-> (isEven(n) || isEven(
    succ(n)
  ))

  val base: Term = claim(zero).incl1(zeroEven) !: claim(zero)

  val hyp1: Term = "n-is-Even" :: isEven(n)

  val hyp2: Term = "(n+1)-is-Even" :: isEven(succ(n))

  val thm: GenFuncTyp[Term, Term] = n ~>: (claim(n))

  val step: FuncLike[Term, Func[Term, Term]] = n :~> {
    (claim(n).rec(claim(succ(n)))) {
      hyp1 :-> (claim(succ(n)).incl2(plusTwoEven(n)(hyp1)))
    } {
      hyp2 :-> (claim(succ(n)).incl1((hyp2)))
    }
  }

  val inductor: Func[Term, Func[
    FuncLike[Term, Func[Term, Term]],
    FuncLike[Term, Term]
  ]] = NatInd.induc(claim)

  val pf: FuncLike[Term, Term] = inductor(base)(step) !: thm
}

object LocalConstImpliesConst {
  import Nats.{Nat => Nt, _}

  val A: Typ[Term] = "A" :: Type

  val f: Func[Term,Term] = "f" :: Nt ->: A

  val ass: FuncLike[Term,Equality[Term]] = "assumption" :: n ~>: (f(n) =:= f(succ(n)))

  val claim: Func[Term,IdentityTyp[Term]] = n :-> (f(zero) =:= f(n))

  val base: Refl[Term] = f(zero).refl

  val hyp: Equality[Term] = "hypothesis" :: (f(zero) =:= f(n))
  val step: Func[Equality[Term],Equality[Term]] = hyp :-> {
    IdentityTyp.trans(A)(f(zero))(f(n))(f(succ(n)))(hyp)(ass(n))
  }

  val thm: GenFuncTyp[Term,Equality[Term]] = n ~>: (claim(n))

  val inductor: Func[Equality[Term],Func[FuncLike[Term,Func[Equality[Term],Equality[Term]]],FuncLike[Term,Equality[Term]]]] = NatInd.induc(claim)

  val pf: FuncLike[Term,Equality[Term]] = inductor(base)(n :~> step) !: thm
}
