import provingground._
import HoTT._

import TLImplicits._
import shapeless.{Nat => SNat, _}
val Nat = "Nat" :: Type
val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
val zero :: succ :: HNil = NatInd.intros
val one = succ(zero)
val two = succ(one)
val three = succ(two)
val four = succ(three)
val five = succ(four)
val six = succ(five)
val n = "n" :: Nat
val m = "m" :: Nat
val m1 = "m1" :: Nat
val m2 = "m2" :: Nat
val recNNN = NatInd.rec(Nat ->: Nat)
val addn = "add(n)" :: Nat ->: Nat
val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))
val recNNNN = NatInd.rec(Nat ->: Nat ->: Nat)
val fibn = "fib_aux(n,_,_)" :: Nat ->: Nat ->: Nat
val fib_aux = recNNNN(m1 :-> (m2 :-> m1))(n :-> (fibn :-> (m1 :-> (m2 :-> fibn(m2)(add(m1)(m2)) ))))
val fib = n :-> fib_aux(n)(zero)(one)
val stepData = (n :-> (fibn :-> (m1 :-> (m2 :-> fibn(m2)(add(m1)(m2)) ))))

val Nt = "Nat" :: Type
val k = "k" :: Nt
val l = "l" :: Nt


lazy val fadd = "add" :: Nt ->: Nt ->: Nt
lazy val ffib_aux = recNNNN(m1 :-> (m2 :-> m1))(n :-> (fibn :-> (m1 :-> (m2 :-> fibn(m2)(fadd(m1)(m2)) ))))

lazy val fstepData = (n :-> (fibn :-> (m1 :-> (m2 :-> fibn(m2)(fadd(m1)(m2)) ))))



def fn(t: Term) = FormalAppln.unapply(t).get._1
def arg(t: Term) = FormalAppln.unapply(t).get._1
def dc(t: Term) = t match {case d : RecursiveDefinition.DataCons[u, v, w] => d}

lazy val recres = ffib_aux(succ(n))
lazy val psi = fn(fn(fn(recres(m1)(m2)))) // equal to ffib_aux
lazy val result = fstepData(succ(n))(recres)
lazy val eta = fn(fn(fn(result(m1)(m2)))) // not equal to psi; but allegedly equal to ffib_aux

lazy val fibd = dc(dc(ffib_aux).tail).data
lazy val etad = dc(dc(eta).tail).data  // badly mangled, has a variable in scope that is not a lambda.
lazy val psid = dc(dc(psi).tail).data
// It is claimed that fibd == etad, but they are clearly different
