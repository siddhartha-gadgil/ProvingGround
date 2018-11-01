package provingground.library
// some imports
import provingground._, HoTT._
import scalahott._, andrewscurtis.FreeGroups._
import spire.implicits._
import NatRing.{ x=>_,  Literal => nat, _}, QField.{w => _, x =>_, y=>_, z=>_, Literal => rat, _}, FreeGroup.{Literal => elem, _}
import Theorems.{PowerDistributive, ConjPower}

object PolymathProof{

// The length function, and the "witnesses" for assumptions on it

val l = "l" :: FreeGroup ->: QTyp
val g = "g" :: FreeGroup
val h = "h" :: FreeGroup
val n = "n" :: NatTyp

 val triang =
         "triangle-inequality" :: (
           g ~>: (h ~>: (
             (leq(l(g |+| h))(l(g) + l(h)))
           ))
         )

 val conjInv =
         "conjugacy-invariance" :: (
           g ~>: (
             h ~>: (
               (l(h) =:= (l(g |+| h |+| g.inverse)))
             )
           )
         )

// variables for conjugacies
 val w = "w" :: FreeGroup
 val y = "y" :: FreeGroup
 val z = "z" :: FreeGroup
 val s = "s" :: FreeGroup
 val t = "t" :: FreeGroup

// terms for the internal repetition trick
 val wy = w |+| y
 val zwbar = z |+| w.inverse
 val wyn = FreeGroup.power(wy)(n)
 val zwbarn = FreeGroup.power(zwbar)(n)

 val c = n :-> (wyn |+| s.inverse |+| t |+| zwbarn) // this is the function we have to bound.
 val r = incl(QField)
 val f = n :-> (l(s.inverse) + l(t) + ((l(y) + l(z)) * r(n) ) )

 //Introducing the lemma
 val lemma = n :-> (leq (l(c(n)) )(f(n) ) )

//Proving that d(n) = c(n+1) with the value dIsc
 val d = n :-> (wy |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| zwbar)
 val dIsc = FreeGroup.rm(s.inverse |+| t |+| zwbarn |+| zwbar) *: (PowerDistributive.pf(wy)(nat(1))(n))
 assert(dIsc.typ == (d(n) =:= c(succ(n))))

 //Conjugacy reduced form of d(n)
 val b = n :-> (y |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| z)

//Use conjugacy invariance to prove l(b(n)) = l(c(n+1)) with bIsc
 val lbIsld = conjInv(w)(y |+| c(n) |+| z)
 val bIsc = lbIsld && (l *: dIsc)
 assert(bIsc.typ == (l(b(n)) =:= l(c(succ(n))) ))

 // Storing the base case
 val baseCase = triang(inv(s))(t) !: (lemma(0))

 val hyp = "hyp" :: lemma(n) //Induction hypothesis

 //bound l(b(n)) with l(c(n))
 val lbnBoundedlcnlylz = triang(y)(c(n)) + triang(y |+| c(n))(z)
 assert(lbnBoundedlcnlylz.typ == (leq(l(b(n)))(l(c(n)) + l(y) + l(z))))

// Use Ind Hyp to bound l(b(n))
 val lbnBounded = lbnBoundedlcnlylz + hyp
 assert(lbnBounded.typ == leq(l(b(n)) )(f(succ(n))) )

 //bound l(c(n+1))
 val bnd = "bound" :: QField.LocalTyp
 val cbnd = bIsc.lift(bnd :-> (leq(bnd)(f(succ(n)) ) ))
 val step = cbnd(lbnBounded)
 assert(step.typ == lemma(succ(n)))

//Complete proof of lemma by induction
 val lemmaProof = Induc(lemma, baseCase, n :~> (hyp :-> step))
 assert(lemmaProof.typ == (n ~>: (lemma(n))) )

 //introduce relevant terms witnessing the setup and the two expressions for x
val x = "x" :: FreeGroup
val grpe = "grpe" :: FreeGroup
val pown = grpe :-> FreeGroup.power(grpe)(n)
val c1 = "x ~ wy" :: (x =:= (s |+| w |+| y |+| s.inverse))
val c2 = "x ~ zw^{-1}" :: (x =:= (t |+| z |+| w.inverse |+| t.inverse))

//Use inbuilt thm for conjugate powers
val xnConjwyn = (pown *: c1) && ConjPower.pf(s)(wy)(n)
val xnConjzwbarn= (pown *: c2) && ConjPower.pf(t)(zwbar)(n)
assert(xnConjwyn.typ == (pown(x) =:= (s |+| pown(wy)  |+| s.inverse  ) ) )
assert(xnConjzwbarn.typ == (pown(x) =:= (t |+| pown(zwbar)  |+| t.inverse  ) ) )

// Multiply x^n in one form with the same in the other form
val t1 = s |+| pown(wy)  |+| s.inverse
val t2 = t |+| pown(zwbar)  |+| t.inverse
val xnxnExpr = (FreeGroup.rm(pown(x)) *: xnConjwyn) && (FreeGroup.lm(t1) *: xnConjzwbarn)
assert(xnxnExpr.typ == ((pown(x) |+| pown(x)) =:= (t1 |+| t2 )))

//assert equality to x^2n
val x2nExpr =PowerDistributive.pf(x)(n)(n).sym && xnxnExpr
assert(x2nExpr.typ == (FreeGroup.power(x)(NatRing.prod(n)(nat(2))) =:= (s |+| c(n) |+| t.inverse)))

//get bounds for the expression for x^2n
val thmBound = f(n) + l(s) + l(t.inverse)
val exprBound = lemmaProof(n) + triang(s)(c(n)) + triang(s |+| c(n))(t.inverse)
assert(exprBound.typ == leq(l(s |+| c(n) |+| t.inverse ))(thmBound))

//Use equality fo x^2n and its expression to bound x^2n
val thmProof = x2nExpr.sym.lift (grpe :-> leq(l(grpe))(thmBound))(exprBound)
val x2n = FreeGroup.power(x)(NatRing.prod(n)(nat(2)))
assert(thmProof.typ == leq(l(x2n))(thmBound ))

}
