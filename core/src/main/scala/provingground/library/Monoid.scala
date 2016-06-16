package provingground.library

import provingground._

import HoTT._

object Monoid {
  val M = "M" :: Type
  
  val sym = IdentityTyp.symm(M)
  
  val trans = IdentityTyp.trans(M)
  
  val a = "a" :: M
  val b = "b" :: M
  val c = "c" :: M
  
  val l = "e_l" :: M
  
  val r = "e_r" :: M
  
  val op = "_*_" :: M ->: M ->: M
  
  val leftId = (a ~>: (op(l)(a) =:= a)).Var
  val rightId = (a ~>: (op(a)(r) =:= a)).Var
  
  val refl = lmbda(a)(Refl(M, a))
  
  val assoc = 
    (a ~>: (b ~>: (c ~>: (
      op(a)(op(b)(c)) =:= op(op(a)(b))(c)
      )))).Var 

  val dist = FiniteDistribution.unif(M, a, b, c, l, r, op, leftId, rightId, refl, assoc)
  
  val Vars = Vector[Term](a, b, c) map (Weighted(_, 1.0/3.0))
}