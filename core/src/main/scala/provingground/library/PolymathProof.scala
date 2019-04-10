package provingground.library
// some imports
import provingground._
import HoTT._
import scalahott.{NatRing, QField, _}
import andrewscurtis.FreeGroups._
import spire.implicits._
import NatRing.{Literal => nat, leq => leqNat, x => _, _}
import QField.{Literal => rat, w => _, x => _, y => _, z => _, _}
import FreeGroup.{Literal => elem, _}
import Theorems.{ConjPower, PowerDistributive}
import spire.math.{Rational, SafeLong}

object PolymathProof {

// The length function, and the "witnesses" for assumptions on it

  val l: Func[RepTerm[Word], RepTerm[Rational]] = "l" :: FreeGroup ->: QTyp
  val g: RepTerm[Word] = "g" :: FreeGroup
  val h: RepTerm[Word] = "h" :: FreeGroup
  val n: RepTerm[SafeLong] = "n" :: NatTyp

  val triang: FuncLike[RepTerm[Word], FuncLike[RepTerm[Word], PosWit]] =
    "triangle-inequality" :: (
      g ~>: (h ~>: (
        (leq(l(g |+| h))(l(g) + l(h)))
      ))
    )

  val conjInv: FuncLike[RepTerm[Word], FuncLike[RepTerm[Word], Equality[RepTerm[Rational]]]] =
    "conjugacy-invariance" :: (
      g ~>: (
        h ~>: (
          (l(h) =:= (l(g |+| h |+| g.inverse)))
        )
      )
    )

// variables for conjugacies
  val w: RepTerm[Word] = "w" :: FreeGroup
  val y: RepTerm[Word] = "y" :: FreeGroup
  val z: RepTerm[Word] = "z" :: FreeGroup
  val s: RepTerm[Word] = "s" :: FreeGroup
  val t: RepTerm[Word] = "t" :: FreeGroup

// terms for the internal repetition trick
  val wy: RepTerm[Word] = w |+| y
  val zwbar: RepTerm[Word] = z |+| w.inverse
  val wyn: RepTerm[Word] = FreeGroup.power(wy)(n)
  val zwbarn: RepTerm[Word] = FreeGroup.power(zwbar)(n)

  val c: Func[RepTerm[SafeLong], RepTerm[Word]] = n :-> (wyn |+| s.inverse |+| t |+| zwbarn) // this is the function we have to bound.
  val r: Func[NatRing.LocalTerm, QField.LocalTerm] = incl(QField)
  val f: Func[RepTerm[SafeLong], RepTerm[Rational]] = n :-> (l(s.inverse) + l(t) + ((l(y) + l(z)) * r(n)))

  //Introducing the lemma
  val lemma: Func[RepTerm[SafeLong], Pos] = n :-> (leq(l(c(n)))(f(n)))

//Proving that d(n) = c(n+1) with the value dIsc
  val d: Func[RepTerm[SafeLong], RepTerm[Word]] = n :-> (wy |+| wyn |+| s.inverse |+| t |+| zwbarn |+| zwbar)
  val dIsc: Equality[RepTerm[Word]] = FreeGroup.rm(s.inverse |+| t |+| zwbarn |+| zwbar) *: (PowerDistributive
    .pf(wy)(nat(1))(n))
  assert(dIsc.typ == (d(n) =:= c(succ(n))))

  //Conjugacy reduced form of d(n)
  val b: Func[RepTerm[SafeLong], RepTerm[Word]] = n :-> (y |+| wyn |+| s.inverse |+| t |+| zwbarn |+| z)

//Use conjugacy invariance to prove l(b(n)) = l(c(n+1)) with bIsc
  val lbIsld: Equality[RepTerm[Rational]] = conjInv(w)(y |+| c(n) |+| z)
  val bIsc: Equality[RepTerm[Rational]] = lbIsld && (l *: dIsc)
  assert(bIsc.typ == (l(b(n)) =:= l(c(succ(n)))))

  // Storing the base case
  val baseCase: PosWit = triang(inv(s))(t) !: (lemma(0))

  val hyp: PosWit = "hyp" :: lemma(n) //Induction hypothesis

  //bound l(b(n)) with l(c(n))
  val lbnBoundedlcnlylz: PosWitSum = triang(y)(c(n)) + triang(y |+| c(n))(z)
  assert(lbnBoundedlcnlylz.typ == (leq(l(b(n)))(l(c(n)) + l(y) + l(z))))

// Use Ind Hyp to bound l(b(n))
  val lbnBounded: PosWitSum = lbnBoundedlcnlylz + hyp
  assert(lbnBounded.typ == leq(l(b(n)))(f(succ(n))))

  //bound l(c(n+1))
  val bnd: RepTerm[Rational] = "bound" :: QField.LocalTyp
  val cbnd: Func[PosWit, PosWit] = bIsc.lift(bnd :-> (leq(bnd)(f(succ(n)))))
  val step: PosWit = cbnd(lbnBounded)
  assert(step.typ == lemma(succ(n)))

//Complete proof of lemma by induction
  val lemmaProof = Induc(lemma, baseCase, n :~> (hyp :-> step))
  assert(lemmaProof.typ == (n ~>: (lemma(n))))

  //introduce relevant terms witnessing the setup and the two expressions for x
  val x: RepTerm[Word] = "x" :: FreeGroup
  val grpe: RepTerm[Word] = "grpe" :: FreeGroup
  val pown: Func[RepTerm[Word], RepTerm[Word]] = grpe :-> FreeGroup.power(grpe)(n)
  val c1: Equality[RepTerm[Word]] = "x ~ wy" :: (x =:= (s |+| w |+| y |+| s.inverse))
  val c2: Equality[RepTerm[Word]] = "x ~ zw^{-1}" :: (x =:= (t |+| z |+| w.inverse |+| t.inverse))

//Use inbuilt thm for conjugate powers
  val xnConjwyn: Equality[RepTerm[Word]] = (pown *: c1) && ConjPower.pf(s)(wy)(n)
  val xnConjzwbarn: Equality[RepTerm[Word]] = (pown *: c2) && ConjPower.pf(t)(zwbar)(n)
  assert(xnConjwyn.typ == (pown(x) =:= (s |+| pown(wy) |+| s.inverse)))
  assert(xnConjzwbarn.typ == (pown(x) =:= (t |+| pown(zwbar) |+| t.inverse)))

// Multiply x^n in one form with the same in the other form
  val t1: RepTerm[Word] = s |+| pown(wy) |+| s.inverse
  val t2: RepTerm[Word] = t |+| pown(zwbar) |+| t.inverse
  val xnxnExpr: Equality[RepTerm[Word]] = (FreeGroup.rm(pown(x)) *: xnConjwyn) && (FreeGroup.lm(t1) *: xnConjzwbarn)
  assert(xnxnExpr.typ == ((pown(x) |+| pown(x)) =:= (t1 |+| t2)))

//assert equality to x^2n
  val x2nExpr: Equality[RepTerm[Word]] = PowerDistributive.pf(x)(n)(n).sym && xnxnExpr
  assert(
    x2nExpr.typ == (FreeGroup
      .power(x)(NatRing.prod(n)(nat(2))) =:= (s |+| c(n) |+| t.inverse)))

//get bounds for the expression for x^2n
  val thmBound: RepTerm[Rational] = f(n) + l(s) + l(t.inverse)
  val exprBound: PosWitSum = lemmaProof(n) + triang(s)(c(n)) + triang(s |+| c(n))(
    t.inverse)
  assert(exprBound.typ == leq(l(s |+| c(n) |+| t.inverse))(thmBound))

//Use equality fo x^2n and its expression to bound x^2n
  val thmProof: PosWit = x2nExpr.sym.lift(grpe :-> leq(l(grpe))(thmBound))(exprBound)
  val x2n: RepTerm[Word] = FreeGroup.power(x)(NatRing.prod(n)(nat(2)))
  assert(thmProof.typ == leq(l(x2n))(thmBound))

}
