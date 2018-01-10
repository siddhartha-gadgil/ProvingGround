import functionfinder._, andrewscurtis.FreeGroups._
import spire.implicits._
import LinNormBound.{x => _, y =>_, _}
import NatRing.{ x=>_,  Literal => nat, _}, QField.{w => _, x=>_, y=>_, Literal => rat, _}, FreeGroup.{Literal => elem, _}
val w = "w" :: FreeGroup
val x = "x" :: FreeGroup
val y = "y" :: FreeGroup
val s = "s" :: FreeGroup
val t = "t" :: FreeGroup

val wx = w |+| x
val ywbar = y |+| w.inverse
val wxn = FreeGroup.power(wx)(n)
val ywbarn = FreeGroup.power(ywbar)(n)

val c = n :-> (wxn |+| s.inverse |+| t |+| ywbarn)

import Theorems.PowerDistributive

val d = n :-> (wx |+| wxn |+| s.inverse |+| t |+| ywbarn  |+| ywbar)
val b = n :-> (x |+| wxn |+| s.inverse |+| t |+| ywbarn  |+| y)
val pn = FreeGroup.rm(s.inverse |+| t |+| ywbarn |+| ywbar) *: (PowerDistributive.pf(wx)(nat(1))(n))
assert(pn.typ == (d(n) =:= c(succ(n))))
val cin = conjInv(w)(x |+| c(n) |+| y)

val r = incl(QField)
val f = n :-> (l(s.inverse) + l(t) + ((l(x) + l(y)) * r(n) ) )

val lemma = n :-> (leq (l(c(n)) )(f(n) ) )
val base = triang(inv(s))(t) !: (lemma(0))
