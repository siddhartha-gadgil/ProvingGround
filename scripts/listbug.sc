import implicits._
val A = "A" :: Type
val B = "B" :: Type
val List = "List" :: Type ->: Type
val ListIndA = ("nil" ::: List(A)) |: ("cons" ::: A ->>: List(A) -->>: List(A)) =: List(A)
val ListInd = A ~->: ListIndA
import shapeless._
val nilA :: consA :: HNil = ListInd(A).intros
val nilB :: consB :: HNil = ListInd(B).intros
val a = "a" :: A
val b = "b" :: B
val la = "l_a" :: List(A)
val lb = "l_b" :: List(B)
val X = "X" :: Type
val recAX = ListInd(A).rec(X)
val x = "x" :: X
(recAX(x)(a :-> (la :-> (x :-> x))).replace(A, B))(nilB)
val f = "f" :: A ->: B
val step = a :-> (la :-> (lb :-> consB(f(a))(lb)))
val recLAB = ListInd(A).rec(List(B))
val mapAB = recLAB(nilB)(step)
val lmap = A :~> (B :~> (f :-> (mapAB)))
import Fold._
val h = "h" :: B->: A
val mpBA = lmap(B)(A)(h)
val bb = consB(b)(nilB)
mpBA(bb)
