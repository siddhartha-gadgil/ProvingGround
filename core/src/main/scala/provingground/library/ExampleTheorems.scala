package provingground.library

import provingground._

import HoTT._

import TLImplicits._

import shapeless._

object SimpleEvens{
  import Nats.{Nat => Nt, _}
  val isEven = "isEven" :: Nt ->: Type
  val zeroEven = "0even" :: isEven(zero)
  val plusTwoEven = "_+2even" :: (n ~>: (isEven(n) ->: isEven(succ(succ(n)))))
}

object DoubleEven{
  import Nats.{Nat => Nt, _}

  import SimpleEvens._

  val thm = n ~>: isEven(double(n))

  val hyp = "isEven(double(n))" :: isEven(double(n))

  val inductor = NatInd.induc(n :-> isEven(double(n)))

  val pf =
    inductor(
       zeroEven){
         n :~> (
           hyp :-> (
             plusTwoEven(double(n))(hyp)
             )
             )
       }  !: thm
}

object SuccNOrNEven{
  import SimpleEvens._

  import Nats.{Nat => Nt, _}

  val statement = n :-> (isEven(n) || isEven(succ(n)))

  val base = statement(zero).incl1(zeroEven) !: statement(zero)

  val hyp1 = "n-is-Even" :: isEven(n)

  val hyp2 = "(n+1)-is-Even" :: isEven(succ(n))

  val thm = n ~>: (statement(n))

  val step = n :~> {
    (statement(n).rec(statement(succ(n)))){
      hyp1 :-> (statement(succ(n)).incl2(plusTwoEven(n)(hyp1)))}{
        hyp2 :-> (statement(succ(n)).incl1((hyp2)))}
      }

  val inductor = NatInd.induc(statement)

  val pf = inductor(base)(step) !: thm
}
