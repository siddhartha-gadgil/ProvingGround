package provingground

import edu.stanford.nlp.trees.Tree

import translation._

/**
  * Expression in a language to represent terms in HoTT and
  * trees representing them in prose, including partially collapsed trees.
  * At the level of structure, we do not make distinctions between words and phrases;
  * the distinction is specific to trees, where a word is a leaf.
  *
  *
  * We model expressions that will mostly become Terms in HoTT, mostly following Naproche.
  * At the upper level are sentential phrases. We typically start with a tree that encodes a sentential phrase.
  *
  */
sealed trait MathExpr

/**
  * Expression in a language to represent terms in HoTT and
  * trees representing them in prose, including partially collapsed trees.
  * At the level of structure, we do not make distinctions between words and phrases;
  * the distinction is specific to trees, where a word is a leaf.
  *
  *
  * We model expressions that will mostly become Terms in HoTT, mostly following Naproche.
  * At the upper level are sentential phrases. We typically start with a tree that encodes a sentential phrase.
  *
  *  We have various case classes for syntax, but a single trait
  *
  */
object MathExpr {

  type T = Tree

  /**
    * An abstract sentential phrase, representing a term.
    */
  // sealed trait SententialPhrase extends MathExpr

  type SententialPhrase = MathExpr

  /**
    * A single tree for a sentential phrase
    */
  case class SP(spv: Vector[MathExpr]) extends SententialPhrase

  /**
    * A conjunction of  sentential phrases forming a sentential phrase.
    */
  case class ConjuctSP(sps: SententialPhrase) extends SententialPhrase

  /**
    * A disjunction of sentential phrases forming a sentential phrase.
    */
  case class DisjuctSP(sps: SententialPhrase) extends SententialPhrase

  /**
    * An if_then_ sentential phrase, translating to implies.
    */
  case class IfThen(premise: SententialPhrase, consequence: SententialPhrase)
      extends SententialPhrase

  /**
    * A bi-implication, kept separate from two implications for definitions etc
    */
  case class Iff(premise: SententialPhrase, consequence: SententialPhrase)
      extends SententialPhrase

  /**
    * Pronoun 'it' with an optional coreference
    */
  case class It(coref: Option[MathExpr]) extends MathExpr

  /**
    * Pronoun 'they' with (psoosibly empty) coreferences
    */
  case class They(corefs: Vector[MathExpr]) extends MathExpr

  /**
    * Abstract Noun phrase
    */
  // sealed trait NounPhrase extends MathExpr
  type NounPhrase = MathExpr

  /**
    * A Noun phrase as a single tree.
    */
  case class NP(npv: Vector[MathExpr]) extends NounPhrase

  case class NN(word: String) extends NounPhrase

  case class Formula(text: String) extends NounPhrase {
    def dp = DP(Determiner.Zero, Vector(), Some(this))
  }

  /**
    * A noun phrase that is a conjuction (and) of noun phrases.
    */
  case class ConjunctNP(nps: Vector[NounPhrase]) extends NounPhrase

  /**
    * A noun phrase  that is a disjunction (or) of noun phrases.
    */
  case class DisjunctNP(nps: Vector[NounPhrase]) extends NounPhrase

  /**
    * Abstract verb phrase
    */
  // sealed trait VerbPhrase extends MathExpr
  type VerbPhrase = MathExpr

  /**
    * A Verb phrase as a single tree
    */
  case class VP(vpv: Vector[MathExpr]) extends VerbPhrase

  case class VB(wrod: String) extends VerbPhrase

  /**
    * Negated verb phrase
    */
  case class NegVP(vp: VerbPhrase) extends VerbPhrase

  /**
    * A verb phrase formed by a transitive  verb applied to its object.
    */
  case class VerbObj(vp: VerbPhrase, obj: NounPhrase) extends VerbPhrase

  /**
    * A NP-VP decomposed sentential phrase
    */
  case class NPVP(np: NounPhrase, vp: VerbPhrase) extends SententialPhrase

  // object NPVP {
  //   type W = (MathExpr[T], MathExpr[T])
  //   implicit val incl = new SubType[NPVP, W] {
  //     def incl[T] = { case NPVP(np, vp) => (np, vp) }
  //
  //     def restrict[T] = {
  //       case Some((np: NounPhrase[T], vp: VerbPhrase[T])) => Some(NPVP(np, vp))
  //       case _                                            => None
  //     }
  //   }
  // }

  /**
    * Various prepositions
    */
  object Preposition {
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
  }

  /**
    * A generic preposition
    */
  case class Prep(word: String) extends Preposition

  /**
    * Preposition - this is a closed class.
    */
  // sealed trait Preposition

  type Preposition = MathExpr

  object Determiner {
    case object A extends Determiner

    case object The extends Determiner

    case object That extends Determiner

    case object Some extends Determiner

    case object Every extends Determiner

    case object Zero extends Determiner

    case object No extends Determiner

    case object This extends Determiner

    case class Card(s: String) extends Determiner

    def apply(s: String) = s.toLowerCase match {
      case "a"                    => A
      case "an"                   => A
      case "the"                  => The
      case "some"                 => Some
      case "every"                => Every
      case "all"                  => Every
      case "no"                   => No
      case "any"                  => Every
      case "this"                 => This
      case "that"                 => That
      case s if s.startsWith("#") => Card(s.drop(1))
    }
  }

  /**
    * Determiner - this is a closed class.
    */
  // sealed trait Determiner

  type Determiner = MathExpr

  /**
    * Abstract adjectival phrase
    */
  // sealed trait AdjectivalPhrase extends MathExpr
  type AdjectivalPhrase = MathExpr

  /**
    * Adjectival phrase as a singl tree - usually just an adjective
    */
  case class AP(ap: T) extends AdjectivalPhrase

  case class JJ(word: String) extends AdjectivalPhrase

  case class JJPP(adj: MathExpr, pps: Vector[MathExpr])
      extends AdjectivalPhrase

  /**
    * Prepositional phrase, usually translates to an argument, sometimes to a property.
    * Can be a Post-modifier in a determiner phrase, where it acts as an argument.
    */
  case class PP(negated: Boolean = false, prep: Preposition, np: NounPhrase)
      extends PostModifier

  /**
    * A quantterm (in the Naproche sense) - a variable or something more complex such as `n_k`.
    * Unlike Naproche, we may also have pronouns.
    */
  // sealed trait QuantTerm extends MathExpr
  type QuantTerm = MathExpr

  /**
    * A variable, the simplest form of QuantTerm
    */
  case class Variable(variable: T) extends QuantTerm

  /**
    * Post-modifier in a determiner phrase.
    */
  // sealed trait PostModifier extends MathExpr
  type PostModifier = MathExpr

  /**
    * A such that clause
    */
  case class SuchThat(condition: MathExpr) extends PostModifier

  /**
    * Determiner phrase: this is the main composite tree.
    */
  case class DP(det: Determiner,
                adjectives: Vector[AdjectivalPhrase] = Vector(),
                optNoun: Option[NounPhrase] = None,
                quantTerms: Option[NounPhrase] = None,
                post: Vector[PostModifier] = Vector())
      extends MathExpr {
    def st(wh: MathExpr) = this.copy(post = post :+ SuchThat(wh))

    def add(pp: PostModifier) =
      this.copy(post = this.post :+ pp)
  }

  /**
    * The core of a determiner phrase, which is a noun, a list of quant-terms
    * or a noun followed by a list of quant-terms
    */
  case class Core(optNoun: Option[NounPhrase],
                  quantterms: Vector[QuantTerm] = Vector())
      extends MathExpr

  // case class QuantifiedNoun(np: NounPhrase)

  /**
    * A transitive verb and adjective.
    */
  case class VerbAdj(vp: VerbPhrase, ap: AdjectivalPhrase) extends VerbPhrase

  /**
    * representing existential 'there', purely as a parsing target
    */
  case object Exists extends MathExpr

  /**
    * to take care of an idiosyncracy of the StanfordParser, with 'if' SP attached to a VP in an NPVP
    */
  case class VPIf(vp: VerbPhrase, ifc: MathExpr) extends MathExpr

  /**
    * is ... property given by a noun.
    */
  case class IsNoun(property: NounPhrase) extends VerbPhrase // case of VerbObj

  /**
    * is ... property given by an intransitive adjective.
    */
  case class IsAdj(coProperty: AdjectivalPhrase) extends VerbPhrase

  /**
    * are ... property given by a transitive adjective, e.e., independent.
    */
  case class AreAdj(coProperty: AdjectivalPhrase) extends VerbPhrase

  // case class VerbAdj(verb: MathExpr, adjs : Vector[MathExpr]) extends VerbPhrase

  /**
    * is ... prep ... property given by a transitive adjective
    */
  case class IsAdjPrep(adj: AdjectivalPhrase, pp: PP) extends VerbPhrase

  /**
    * is such that ... property
    */
  case class IsSuchThat(st: SuchThat) extends VerbPhrase

  /**
    * is ... property given by a prespositional phrase
    */
  case class IsPrep(pp: PP) extends VerbPhrase

  case class VerbPP(verb: MathExpr, pps: Vector[MathExpr]) extends VerbPhrase

  /**
    * Universally quantified sentence.
    */
  case class ForAllSP(sentence: SententialPhrase) extends SententialPhrase

  /**
    * Existential-style quantified sentence, but also allowing at most one and precisely one.
    */
  case class ExistentialSP(sentence: SententialPhrase,
                           exists: Boolean = true,
                           unique: Boolean = false)
      extends SententialPhrase

  /**
    * Negation of a sentence.
    */
  case class NegSP(sentence: SententialPhrase) extends SententialPhrase

  /**
    * Conjunction of sentential phrases.
    */
  case class ConjSP(sentences: Vector[SententialPhrase])
      extends SententialPhrase

  /**
    * Disjunction of sentential phrases.
    */
  case class DisjSP(sentences: Vector[SententialPhrase])
      extends SententialPhrase

  /**
    * Sentential phrases connected by "i.e., " or "so, ".
    */
  case class ThatIsSP(sentences: Vector[SententialPhrase])
      extends SententialPhrase
}

object FormalExpr {
  case class FormalLeaf(s: String) extends MathExpr

  case class FormalNode(s: String, children: Vector[MathExpr]) extends MathExpr

  import Translator.Pattern

  import Functors._

  // import cats._

  import cats.implicits._

  type SL[A] = (S[A], Vector[A])

  // implicit val travSL = traversePair[S, Vector]

  case class Vec(vec: Vector[MathExpr]) extends MathExpr

  def translator =
    Translator.Empty[Tree, MathExpr] || Pattern.partial[Tree, S] {
      case PennTrees.Leaf(s) => s
    } >>> {
      case s => FormalLeaf(s)
    } ||
      Pattern.partial[Tree, SL] {
        case PennTrees.Node(s, l) => (s, l)
      } >>> {
        case ("NP", Vector(dp: MathExpr.DP)) => dp
        case (s, l)                          => FormalNode(s, l)
      }
}

/**
  * An unparsed tree, the default parse for iterative debugging.
  */
case class Raw(model: TreeModel) extends MathText with MathExpr
// with NounPhrase
// with VerbPhrase
// with SententialPhrase
// with Preposition
// with AdjectivalPhrase
// with Determiner
// with PostModifier
// with QuantTerm

object Raw {
  val translator = Translator.Simple[Tree, MathExpr]((t: Tree) =>
    Some(Raw(PennTrees.model(t))))
}

sealed trait MathText

object MathText {

  import MathExpr._

  case class Assert(assertion: SententialPhrase) extends MathText

  case class Assume(assumption: SententialPhrase) extends MathText

  case class BiImplicationDefiniendumSP(definiendum: SententialPhrase)
      extends SententialPhrase

  case class BiImplicationDefiniendum(name: String,
                                      variables: Vector[T],
                                      formula: SententialPhrase)
      extends MathExpr

  case class CopulaDefiniendumNP(definiendum: NounPhrase) extends NounPhrase

  case class CopulaDefiniendum(name: String,
                               variables: Vector[T],
                               lhs: NounPhrase)
      extends MathExpr

  case class BiEquationalDefinitionSP(definiendum: BiEquationalDefinitionSP,
                                      definiens: SententialPhrase)
      extends SententialPhrase

  case class CopulaDefinitionSP(definiendum: CopulaDefiniendumNP,
                                definiens: NounPhrase)
      extends SententialPhrase

  case class BiEquationalDefinition(definiendum: BiEquationalDefinition,
                                    definiens: SententialPhrase)
      extends SententialPhrase

  case class CopulaDefinition(definiendum: CopulaDefiniendum,
                              definiens: NounPhrase)
      extends SententialPhrase

  case class VariableType(variables: Vector[NounPhrase], typ: NounPhrase)
}
