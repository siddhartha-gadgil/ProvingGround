package provingground.library
import provingground._
import HoTT._
import shapeless._
import induction.TLImplicits._

object Fibonacci{
import Nats._
val m1 = "m1" :: Nat
val m2 = "m2" :: Nat

val recNNNN = NatInd.rec(Nat ->: Nat ->: Nat)
val fibn    = "fib_aux(n,_,_)" :: Nat ->: Nat ->: Nat
val fib_aux = recNNNN(m1 :-> (m2 :-> m1))(
  n :-> (fibn :-> (m1 :-> (m2 :-> fibn(m2)(add(m1)(m2))))))
val fib = n :-> fib_aux(n)(zero)(succ(zero))

assert(fib(succ(succ(succ(succ(succ(succ(zero))))))) == succ(succ(succ(succ(succ(succ(succ(succ(zero)))))))))

}
