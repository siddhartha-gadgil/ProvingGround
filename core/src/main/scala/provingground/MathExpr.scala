package provingground

/**
  * Expression in a language to represent terms in HoTT and
  trees representing them in prose, including partially collapsed trees.
  * At the level of structure, we do not make distinctions between words and phrases;
  * the distinction is specific to trees, where a word is a leaf.
  *
  * @tparam T the language - a tree for prose, Term for HoTT
  *
  * We model expressions that will mostly become Terms in HoTT, mostly following Naproche.
  * At the upper level are sentential phrases. We typeically start with a tree that encodes a sentential phrase.
  *
  *  We have various traits representing parts of speech, and case classes for syntax.
  * To handle subtyping, we use a more general form of translation mapping
  * ```
  X[A] => G[Y[A]]
  ```
  * with `Y[A]` having an inclusion into `Z[X[A]]` for a traversable functor `Z`.
  */
sealed trait MathExpr[T]

object MathExpr {

  /**
    * An abstract sentential phrase, representing a term.
    */
  sealed trait SententialPhrase[T] extends MathExpr[T]

  /**
    * A single tree for a sentential phrase
    */
  case class SP[T](sp: T) extends SententialPhrase[T]

  /**
    * A conjunction of  sentential phrases forming a sentential phrase.
    */
  case class ConjuctSP[T](sps: SententialPhrase[T]) extends SententialPhrase[T]

  /**
    * A disjunction of sentential phrases forming a sentential phrase.
    */
  case class DisjuctSP[T](sps: SententialPhrase[T]) extends SententialPhrase[T]

  /**
    * An if_then_ sentential phrase, translating to implies.
    */
  case class IfThen[T](premise: SententialPhrase[T],
                       consequence: SententialPhrase[T])
      extends SententialPhrase[T]

  /**
    * Abstract Noun phrase
    */
  sealed trait NounPhrase[T] extends MathExpr[T]

  /**
    * A Noun phrase as a single tree.
    */
  case class NP[T](np: T) extends NounPhrase[T]

  /**
    * A noun phrase that is a conjuction (and) of noun phrases.
    */
  case class ConjuntNP[T](nps: List[NounPhrase[T]]) extends NounPhrase[T]

  /**
    * A noun phrase  that is a disjunction (or) of noun phrases.
    */
  case class DisjuntNP[T](nps: List[NounPhrase[T]]) extends NounPhrase[T]

  /**
    * Abstract verb phrase
    */
  sealed trait VerbPhrase[T] extends MathExpr[T]

  /**
    * A Verb phrase as a single tree
    */
  case class VP[T](vp: T) extends VerbPhrase[T]

  /**
    * Negated verb phrase
    */
  case class NegVP[T](vp: VerbPhrase[T]) extends VerbPhrase[T]

  /**
    * A verb phrase formed by a transitive  verb applied to its object.
    */
  case class VerbObj[T](vp: VerbPhrase[T], obj: NounPhrase[T])
      extends VerbPhrase[T]

  /**
    * A NP-VP decomposed sentential phrase
    */
  case class NPVP[T](np: NounPhrase[T], vp: VerbPhrase[T])
      extends SententialPhrase[T]

  object NPVP{
    type W[T] = (MathExpr[T], MathExpr[T])
    implicit val incl = new SubType[NPVP, W]{
      def incl[T] = {case NPVP(np, vp) => (np, vp)}

      def restrict[T] = {
        case Some((np :NounPhrase[T], vp : VerbPhrase[T])) => Some(NPVP(np, vp))
        case _ => None}
    }
  }

  object Preposition{
    case object Of extends Preposition

    case object Over extends Preposition

    case object Under extends Preposition

    case object By extends Preposition

    case object With extends Preposition

    case object From extends Preposition

    case object To extends Preposition

    case object Into extends Preposition

    case object In extends Preposition

    case object Modulo extends Preposition

    case class Prep(word: String) extends Preposition

  }


  /**
    * Preposition - this is a closed class.
    */
  sealed trait Preposition

  object Determiner{
    case object A extends Determiner

    case object The extends Determiner

    case object Some extends Determiner

    case object Every extends Determiner

    case object Zero extends Determiner

  }

  /**
    * Determiner - this is a closed class.
    */
  sealed trait Determiner

  /**
    * Abstract adjectival phrase
    */
  sealed trait AdjectivalPhrase[T] extends MathExpr[T]

  /**
    * Adjectival phrase as a singl tree - usually just an adjective
    */
  case class AP[T](ap: T) extends AdjectivalPhrase[T]

  /**
    * Prepositional phrase, usually translates to an argument, sometimes to a property.
    Can be a Post-modifier in a determiner phrase, where it acts as an argument.
    */
  case class PP[T](negated: Boolean = false, prep: Preposition, np: NP[T])
      extends PostModifier[T]

  /**
    * A quantterm (in the Naproche sense) - a variable or something more complex such as `n_k`.
   Unlike Naproche, we may also have pronouns.
    */
  sealed trait QuantTerm[T] extends MathExpr[T]

  /**
    * A variable, the simplest form of QuantTerm
    */
  case class Variable[T](variable: T) extends QuantTerm[T]

  /**
    * Post-modifier in a determiner phrase.
    */
  sealed trait PostModifier[T] extends MathExpr[T]

  /**
    * A such that clause
    */
  case class SuchThat[T](condition: SP[T]) extends PostModifier[T]

  /**
    * Determiner phrase: this is the main composite tree.
    */
  case class DP[T](det: Determiner,
                   adjectives: List[AdjectivalPhrase[T]] = List(),
                   core: Core[T],
                   post: List[PostModifier[T]])
      extends MathExpr[T]

  /**
    * The core of a determiner phrase, which is a noun, a list of quant-terms
   or a noun followed by a list of quant-terms
    */
  case class Core[T](optNoun: Option[NounPhrase[T]],
                     quantterms: List[QuantTerm[T]] = List())

  // case class QuantifiedNoun[T](np: NounPhrase[T])

  /**
    * A transitive verb and adjective.
    */
  case class VerbAdj[T](vp: VerbPhrase[T], AdjectivalPhrase: AP[T])
      extends VerbPhrase[T]

  /**
    * is ... property given by a noun.
    */
  case class IsNoun[T](property: NounPhrase[T]) extends VerbPhrase[T]

  /**
    * is ... property given by an intransitive adjective.
    */
  case class IsAdj[T](coProperty: AdjectivalPhrase[T]) extends VerbPhrase[T]

  /**
    * are ... property given by a transitive adjective, e.e., independent.
    */
  case class AreAdj[T](coProperty: AdjectivalPhrase[T]) extends VerbPhrase[T]

  /**
    * is ... prep ... property given by a transitive adjective
    */
  case class IsAdjPrep[T](adj: AdjectivalPhrase[T], pp: PP[T])
      extends VerbPhrase[T]

  /**
    * is such that ... property
    */
  case class IsSuchThat[T](st: SuchThat[T]) extends VerbPhrase[T]

  /**
    * is ... property given by a prespositional phrase
    */
  case class IsPrep[T](pp: PP[T]) extends VerbPhrase[T]

  /**
    * Universally quantified sentence.
    */
  case class ForAllSP[T](sentence: SententialPhrase[T])
      extends SententialPhrase[T]

  /**
    * Existential-style quantified sentence, but also allowing at most one and precisely one.
    */
  case class ExistentialSP[T](sentence: SententialPhrase[T],
                              exists: Boolean = true,
                              unique: Boolean = false)
      extends SententialPhrase[T]

  /**
    * Negation of a sentence.
    */
  case class NegSP[T](sentence: SententialPhrase[T])
      extends SententialPhrase[T]

  /**
    * Conjunction of sentential phrases.
    */
  case class ConjSP[T](sentences: List[SententialPhrase[T]])
      extends SententialPhrase[T]

  /**
    * Disjunction of sentential phrases.
    */
  case class DisjSP[T](sentences: List[SententialPhrase[T]])
      extends SententialPhrase[T]

  /**
    * Sentential phrases connected by "i.e., " or "so, ".
    */
  case class ThatIsSP[T](sentences: List[SententialPhrase[T]])
      extends SententialPhrase[T]
}

sealed trait MathText[T]

object MathText {
  import MathExpr._

  case class Assert[T](assertion: SententialPhrase[T]) extends MathText[T]

  case class Assume[T](assumption: SententialPhrase[T]) extends MathText[T]

  case class BiImplicationDefiniendumSP[T](definiendum: SententialPhrase[T])
      extends SententialPhrase[T]

  case class BiImplicationDefiniendum[T](name: String,
                                         variables: List[T],
                                         formula: SententialPhrase[T])
      extends MathExpr[T]

  case class CopulaDefiniendumNP[T](definiendum: NounPhrase[T])
      extends NounPhrase[T]

  case class CopulaDefiniendum[T](name: String,
                                  variables: List[T],
                                  lhs: NounPhrase[T])
      extends MathExpr[T]

  case class BiEquationalDefinitionSP[T](
      definiendum: BiEquationalDefinitionSP[T],
      definiens: SententialPhrase[T])
      extends SententialPhrase[T]

  case class CopulaDefinitionSP[T](definiendum: CopulaDefiniendumNP[T],
                                   definiens: NounPhrase[T])
      extends SententialPhrase[T]

  case class BiEquationalDefinition[T](definiendum: BiEquationalDefinition[T],
                                       definiens: SententialPhrase[T])
      extends SententialPhrase[T]

  case class CopulaDefinition[T](definiendum: CopulaDefiniendum[T],
                                 definiens: NounPhrase[T])
      extends SententialPhrase[T]

  case class VariableType[T](
      variables: List[NounPhrase[T]], typ: NounPhrase[T])
}
