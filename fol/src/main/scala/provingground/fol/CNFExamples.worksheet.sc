import provingground.fol._, Formula._

val P = Prop("P")
val Q = Prop("Q")
val R = Prop("R")

val fmla1 = P implies Q

negate(P)
negate(negate(P))

// CNF.fromFormula(P)
