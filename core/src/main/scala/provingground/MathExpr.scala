package provingground

/**
  * A term in HoTT, mainly used for translation
  * @tparam T the language - a tree for prose, Term for HoTT
  */
sealed trait MathExpr[T]

object MathExpr {

  /**
    * A sentential phrase
    */
  case class SP[T](sp: T) extends MathExpr[T]

  /**
    * A Noun phrase
    */
  case class NP[T](np: T) extends MathExpr[T]

  /**
    * A Verb phrase
    */
  case class VP[T](vp: T) extends MathExpr[T]

  /**
    * A NP-VP decomposed sentential phrase
    */
  case class NPVP[T](np: NP[T], vp: VP[T]) extends MathExpr[T]

  class Preposition

  class Determiner

  /**
    * Adjectival phrase - usually just an adjective
    */
  case class AP[T](ap: T) extends MathExpr[T]

  /**
    * Prepositional phrase
    */
  case class PP[T](negated: Boolean, prep: Preposition, np: NP[T]) extends MathExpr[T]

  sealed trait QuantTerm[T] extends MathExpr[T]

  sealed trait PostModifier[T] extends MathExpr[T]

  /**
    * Determiner phrase
    */
  case class DP[T](det: Determiner,
                   adjectives: List[AP[T]] = List(),
                   core: Core[T],
                   post: List[PostModifier[T]])
      extends MathExpr[T]

  case class Core[T](optNoun: NP[T], quantifiers: List[QuantTerm[T]])

  case class QuantifiedNoun[T](np: NP[T])

}
