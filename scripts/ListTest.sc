import implicits._
val A = "A" :: Type
val B = "B" :: Type
val List = "List" :: Type ->: Type
val ListIndA = ("nil" ::: List(A)) |: ("cons" ::: A ->>: List(A) -->>: List(A)) =: List(A)
val ListInd = A ~->: ListIndA
ListInd(A).intros.subst(A, B) == ListInd(B).intros
import shapeless._

val nilA :: consA :: HNil = ListInd(A).intros
val nilB :: consB :: HNil = ListInd(B).intros
val f = "f" :: A ->: B
val recLAB = ListInd(A).rec(List(B))

val a = "a" :: A


val la = "l_a" :: List(A)
val lb = "l_b" :: List(B)

val step = a :-> (la :-> (lb :-> consB(f(a))(lb)))
val mapAB = recLAB(nilB)(step)

val a1 = "a1" :: A

mapAB(consA(a)(consA(a1)(nilA))) == consB(f(a))(consB(f(a1))(nilB))
