import functionfinder._, andrewscurtis.FreeGroups._
import spire.implicits._
import LinNormBound.{x => _, y =>_, _}
import NatRing.{ x=>_,  Literal => nat, _}, QField.{w => _, x =>_, y=>_, z=>_, Literal => rat, _}, FreeGroup.{Literal => elem, _}
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

import Theorems.{PowerDistributive, ConjPower}

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

val bnd = "bound" :: QField.LocalTyp
val cbnd = bc.lift(bnd :-> (leq(bnd)(f(succ(n)) ) ))
val step = cbnd(bbd)
assert(step.typ == lemma(succ(n)))

val proof = Induc(lemma, base, n :~> (hyp :-> step))
assert(proof.typ == (n ~>: (lemma(n))) )

val x = "x" :: FreeGroup
val g = "g" :: FreeGroup
val pown = g :-> FreeGroup.power(g)(n)

val c1 = "x ~ wy" :: (x =:= (s |+| w |+| y |+| s.inverse))
val c2 = "x ~ zw^{-1}" :: (x =:= (t |+| z |+| w.inverse |+| t.inverse))

val eq1 = (pown *: c1) && ConjPower.pf(s)(wy)(n)
val eq2 = (pown *: c2) && ConjPower.pf(t)(zwbar)(n)

assert(eq1.typ == (pown(x) =:= (s |+| pown(wy)  |+| s.inverse  ) ) )
assert(eq2.typ == (pown(x) =:= (t |+| pown(zwbar)  |+| t.inverse  ) ) )

val t1 = s |+| pown(wy)  |+| s.inverse
val t2 = t |+| pown(zwbar)  |+| t.inverse

val eq3 = (FreeGroup.rm(pown(x)) *: eq1) && (FreeGroup.lm(t1) *: eq2 )
assert(eq3.typ == ((pown(x) |+| pown(x)) =:= (t1 |+| t2 )   ))

val eq4 =PowerDistributive.pf(x)(n)(n).sym && eq3
assert(eq4.typ == (FreeGroup.power(x)(NatRing.prod(n)(nat(2))) =:= (s |+| c(n) |+| t.inverse)))

val fullBound = f(n) + l(s) + l(t.inverse)
val cnbound = proof(n) + triang(s)(c(n)) + triang(s |+| c(n))(t.inverse)
assert(cnbound.typ == leq(l(s |+| c(n) |+| t.inverse ))(fullBound))

val fullProof = eq4.sym.lift (g :-> leq(l(g))(fullBound))(cnbound)
val x2n = FreeGroup.power(x)(NatRing.prod(n)(nat(2)))
assert(fullProof.typ == leq(l(x2n))(fullBound )) 
