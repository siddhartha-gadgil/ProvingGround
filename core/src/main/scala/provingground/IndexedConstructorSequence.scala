package provingground

import HoTT._

import scala.language.existentials

import Subst._

abstract class IndexedConstructorSeqMap[C <: Term with Subs[C],
                        H <: Term with Subs[H],
                        RecType <: Term with Subs[RecType],
                        InducType <: Term with Subs[InducType],
                        Intros,
                        F <: Term with Subs[F],
                        Index: Subst,
                        IF <: Term with Subs[IF],
                        IDF <: Term with Subs[IDF],
                        IDFT <: Term with Subs[IDFT]] {

  val family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]

  def recDefn(X: Typ[C]): IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT]

  val W: F

  // type RecType <: Term with Subs[RecType]

  def recDataLambda(X: Typ[C]): IF => RecType

  def rec(X: Typ[C]): RecType =
    recDataLambda(X: Typ[C])(recDefn(X: Typ[C]).iterFunc)

  def inducDefn(fibre: IDFT): IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT]

  // type InducType <: Term with Subs[InducType]

  def inducDataLambda(fibre: IDFT): IDF => InducType

  def induc(fibre: IDFT) =
    inducDataLambda(fibre)(inducDefn(fibre).iterDepFunc)
}
