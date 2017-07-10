package provingground

/**
  * Much of  the richness of HoTT is in the definitions of ``Inductive types`` (and their indexed  counterparts)
  * and of (dependent) functions on these by recursion and induction
  * These are implemented using several layers of recursive definitions and diagonals (i.e., fixed points). In HoTT,
  * recursion and induction are applications of (dependent) functions `rec_W,X` and  `ind_W, Xs` to the ``definition data``.
  *
  * It is useful to capture information regarding inductive types and the recursion and induction functions in scala types. Our implementation
  * is designed to do this.
  *
  * ==Inductive Type Definitions==
  *
  * Inductive types are specified by introduction rules. Each introduction rule is specified in [[ConstructorShape]] (without specifying the type)
  * and [[ConstructorTL]] including the specific type. The full definition is in [[ConstructorSeqTL]].
  *
  * ==Recursion and Induction functions==
  *
  * These are defined recursively, first for each introduction rule and then for the inductive type as a whole. A subtlety is that the
  * scala type of the `rec_W,X` and `induc_W, Xs` functions depends on the scala type of the codomain `X` (or family `Xs`).
  * To make these types visible, some type level calculations using  implicits are done, giving traits [[ConstructorPatternMap]]
  * and [[ConstructorSeqMap]] that have recursive definition of the recursion and induction functions, the former for the case of a
  * single introduction rule. Traits [[ConstructorSeqMapper]] and [[ConstructorPatternMapper]] provide the lifts.
  *
  * ==Indexed Versions==
  *
  * There are indexed versions of all these definitions, to work with indexed inductive type families.
  */
package object induction {
  object implicits extends InductionImplicits with SubstImplicits
}
