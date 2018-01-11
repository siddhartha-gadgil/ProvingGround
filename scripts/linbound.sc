import functionfinder._, andrewscurtis.FreeGroups._
import spire.implicits._
import LinNormBound.{x => _, y =>_, _}
import NatRing.{ x=>_,  Literal => nat, _}, QField.{w => _, y=>_, z=>_, Literal => rat, _}, FreeGroup.{Literal => elem, _}
val w = "w" :: FreeGroup
val y = "y" :: FreeGroup
val z = "z" :: FreeGroup
val s = "s" :: FreeGroup
val t = "t" :: FreeGroup

val wy = w |+| y
val zwbar = z |+| w.inverse
val wyn = FreeGroup.power(wy)(n)
val zwbarn = FreeGroup.power(zwbar)(n)

val c = n :-> (wyn |+| s.inverse |+| t |+| zwbarn)

import Theorems.PowerDistributive

val d = n :-> (wy |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| zwbar)
val b = n :-> (y |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| z)
val dc = FreeGroup.rm(s.inverse |+| t |+| zwbarn |+| zwbar) *: (PowerDistributive.pf(wy)(nat(1))(n))
assert(dc.typ == (d(n) =:= c(succ(n))))

val bd = conjInv(w)(y |+| c(n) |+| z)
val bc = bd && (l *: dc)
assert(bc.typ == (l(b(n)) =:= l(c(succ(n))) ))

val r = incl(QField)
val f = n :-> (l(s.inverse) + l(t) + ((l(y) + l(z)) * r(n) ) )

val lemma = n :-> (leq (l(c(n)) )(f(n) ) )
val base = triang(inv(s))(t) !: (lemma(0))

val bitri = triang(y)(c(n)) + triang(y |+| c(n))(z)
assert(bitri.typ == (leq(l(b(n)))(l(c(n)) + l(y) + l(z))))

val hyp = "hyp" :: lemma(n)
val bbd = bitri + hyp

assert(bbd.typ == leq(l(b(n)) )(f(succ(n))) )

val bnd ="bound" :: QField.LocalTyp
val cbnd = bc.lift(bnd :-> (leq(bnd)(f(succ(n)) ) ))
val step = cbnd(bbd)
assert(step.typ == lemma(succ(n)))

val proof = Induc(lemma, base, n :~> (hyp :-> step))
assert(proof.typ == (n ~>: (lemma(n))) )
()
