import provingground._
import HoTT._
val A ="A" :: Type
import Implicits._
val Bool = "Boolean" :: Type
val boolInduc = "true" ::: Bool |: "false" ::: Bool =: Bool
val List(tt, ff) = boolInduc.intros
tt
ff
boolInduc.rec(Bool)
val recBoolBool = boolInduc.rec(Bool)
recBoolBool.typ
import Fold._
val not = recBoolBool(ff)(tt)
not(ff)
not(tt)
boolInduc
val b= "b" :: Bool
val recBBB = boolInduc.rec(Bool ->: Bool)
recBBB.typ
val and = recBBB(lmbda(b)(b))(lmbda(b)(ff))
and(tt)(tt)
and(tt)(ff)
and(ff)(ff)
and(ff)(tt)
val Nat ="Nat" :: Type
val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
val List(zero, succ) = NatInd.intros
val recNatBool = NatInd.rec(Bool)
recNatBool.typ
val n = "n" :: Nat
val isEven = recNatBool(tt)(lmbda(n)(lmbda(b)(not(b))))
val one = succ(zero)
val two = succ(one)
val three = succ(two)
val four = succ(three)
isEven(two)
isEven(three)
val recNNN = NatInd.rec(Nat ->: Nat)
recNNN.typ
val m = "m" :: Nat
val addn ="add(n)" :: Nat ->: Nat
val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> (succ(addn(m))) ) ) )
add(two)(one)
add(two)(one) == three
add(two)(two) == four
