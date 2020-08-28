package provingground.translation
import provingground._

import edu.stanford.nlp.trees.Tree

import translation._

import upickle.default.{ReadWriter => RW, macroRW, _}

import ujson._

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
  implicit def rw: RW[MathExpr] = macroRW

  val tq = "\"\"\""

  type T = Tree

  implicit def rwT: RW[T] =
    readwriter[ujson.Value].bimap[Tree](PennTrees.toJson, PennTrees.fromJson)

  /**
    * An abstract sentential phrase, representing a term.
    */
  // sealed trait SententialPhrase extends MathExpr

  type SententialPhrase = MathExpr

  /**
    * A single tree for a sentential phrase
    */
  case class SP(spv: Vector[MathExpr]) extends SententialPhrase

  object SP {
    implicit def rw: RW[SP] = macroRW
  }

  /**
    * A conjunction of  sentential phrases forming a sentential phrase.
    */
  case class ConjunctSP(sps: Vector[SententialPhrase]) extends SententialPhrase

  object ConjunctSP {
    implicit def rw: RW[ConjunctSP] = macroRW
  }

  /**
    * A disjunction of sentential phrases forming a sentential phrase.
    */
  case class DisjunctSP(sps: Vector[SententialPhrase]) extends SententialPhrase

  object DisjunctSP {
    implicit def rw: RW[DisjunctSP] = macroRW
  }

  /**
    * An if_then_ sentential phrase, translating to implies.
    */
  case class IfThen(premise: SententialPhrase, consequence: SententialPhrase)
      extends SententialPhrase

  object IfThen {
    implicit def rw: RW[IfThen] = macroRW
  }

  /**
    * A bi-implication, kept separate from two implications for definitions etc
    */
  case class Iff(premise: SententialPhrase, consequence: SententialPhrase)
      extends SententialPhrase

  object Iff {
    implicit def rw: RW[Iff] = macroRW
  }

  /**
    * Pronoun 'it' with an optional coreference
    */
  case class It(coref: Option[MathExpr]) extends MathExpr

  object It {
    implicit def rw: RW[It] = macroRW
  }

  /**
    * Pronoun 'they' with (psoosibly empty) coreferences
    */
  case class They(corefs: Vector[MathExpr]) extends MathExpr

  object They {
    implicit def rw: RW[They] = macroRW
  }

  /**
    * Abstract Noun phrase
    */
  // sealed trait NounPhrase extends MathExpr
  type NounPhrase = MathExpr

  /**
    * A Noun phrase as a single tree.
    */
  case class NP(npv: Vector[MathExpr]) extends NounPhrase

  object NP {
    implicit def rw: RW[NP] = macroRW
  }

  case class NN(word: String) extends NounPhrase {
    override def toString = s"NN($tq$word$tq)"
  }

  object NN {
    implicit def rw: RW[NN] = macroRW
  }

  case class Formula(text: String) extends NounPhrase {
    def dp                = DP(Determiner.Zero, Vector(), Some(this))
    override def toString = s"Formula($tq$text$tq)"
  }

  object Formula {
    implicit def rw: RW[Formula] = macroRW
  }

  /**
    * A noun phrase that is a conjuction (and) of noun phrases.
    */
  case class ConjunctNP(nps: Vector[NounPhrase]) extends NounPhrase

  object ConjunctNP {
    implicit def rw: RW[ConjunctNP] = macroRW
  }

  /**
    * A noun phrase  that is a disjunction (or) of noun phrases.
    */
  case class DisjunctNP(nps: Vector[NounPhrase]) extends NounPhrase

  object DisjunctNP {
    implicit def rw: RW[DisjunctNP] = macroRW
  }

  /**
    * Abstract verb phrase
    */
  // sealed trait VerbPhrase extends MathExpr
  type VerbPhrase = MathExpr

  /**
    * A Verb phrase as a single tree
    */
  case class VP(vpv: Vector[MathExpr]) extends VerbPhrase

  object VP {
    implicit def rw: RW[VP] = macroRW
  }

  case class VB(word: String) extends VerbPhrase {
    override def toString = s"VB($tq$word$tq)"
  }

  object VB {
    implicit def rw: RW[VB] = macroRW
  }

  /**
    * Negated verb phrase
    */
  case class NegVP(vp: VerbPhrase) extends VerbPhrase

  object NegVP {
    implicit def rw: RW[NegVP] = macroRW
  }

  /**
    * A verb phrase formed by a transitive  verb applied to its object.
    */
  case class VerbObj(vp: VerbPhrase, obj: NounPhrase) extends VerbPhrase

  object VerbObj {
    implicit def rw: RW[VerbObj] = macroRW
  }

  /**
    * A NP-VP decomposed sentential phrase
    */
  case class NPVP(np: NounPhrase, vp: VerbPhrase) extends SententialPhrase

  object NPVP {
    implicit def rw: RW[NPVP] = macroRW
  }

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

    implicit def rw: RW[Preposition] = macroRW
  }

  /**
    * A generic preposition
    */
  case class Prep(word: String) extends Preposition {
    override def toString = s"Prep($tq$word$tq)"
  }

  object Prep {
    implicit def rw: RW[Prep] = macroRW
  }

  /**
    * Preposition - this is a closed class.
    */
  // sealed trait Preposition

  type Preposition = MathExpr

  object Determiner {
    implicit def rw: RW[Determiner] = macroRW

    case object A extends Determiner

    case object The extends Determiner

    case object That extends Determiner

    case object Some extends Determiner

    case object Every extends Determiner

    case object Zero extends Determiner

    case object No extends Determiner

    case object This extends Determiner

    case class Card(s: String) extends Determiner {
      override def toString = s"Card($tq$s$tq)"
    }

    object Card {
      implicit def rw: RW[Card] = macroRW
    }

    def apply(s: String) = s.toLowerCase match {
      case "a"                    => A
      case "an"                   => A
      case "the"                  => The
      case "some"                 => Some
      case "every"                => Every
      case "all"                  => Every
      case "any"                  => Every
      case "each"                 => Every
      case "no"                   => No
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

  object AP {
    implicit def rw: RW[AP] = macroRW
  }

  case class JJ(word: String) extends AdjectivalPhrase {
    override def toString = s"JJ($tq$word$tq)"
  }

  object JJ {
    implicit def rw: RW[JJ] = macroRW
  }

  case class JJPP(adj: MathExpr, pps: Vector[MathExpr]) extends AdjectivalPhrase

  object JJPP {
    implicit def rw: RW[JJPP] = macroRW
  }

  /**
    * Prepositional phrase, usually translates to an argument, sometimes to a property.
    * Can be a Post-modifier in a determiner phrase, where it acts as an argument.
    */
  case class PP(negated: Boolean = false, prep: Preposition, np: NounPhrase)
      extends PostModifier

  object PP {
    implicit def rw: RW[PP] = macroRW
  }

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

  object Variable {
    implicit def rw: RW[Variable] = macroRW
  }

  /**
    * Post-modifier in a determiner phrase.
    */
  // sealed trait PostModifier extends MathExpr
  type PostModifier = MathExpr

  /**
    * A such that clause
    */
  case class SuchThat(condition: MathExpr) extends PostModifier

  object SuchThat {
    implicit def rw: RW[SuchThat] = macroRW
  }

  case class Which(condition: MathExpr) extends PostModifier

  object Which{
    implicit def rw: RW[Which] = macroRW
  }

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

  object DP {
    implicit def rw: RW[DP] = macroRW
  }

  /**
    * The core of a determiner phrase, which is a noun, a list of quant-terms
    * or a noun followed by a list of quant-terms
    */
  case class Core(optNoun: Option[NounPhrase],
                  quantterms: Vector[QuantTerm] = Vector())
      extends MathExpr

  object Core {
    implicit def rw: RW[Core] = macroRW
  }

  // case class QuantifiedNoun(np: NounPhrase)

  /**
    * A transitive verb and adjective.
    */
  case class VerbAdj(vp: VerbPhrase, ap: AdjectivalPhrase) extends VerbPhrase

  object VerbAdj {
    implicit def rw: RW[VerbAdj] = macroRW
  }

  /**
    * representing existential 'there', purely as a parsing target
    */
  case object Exists extends MathExpr

  /**
    * to take care of an idiosyncracy of the StanfordParser, with 'if' SP attached to a VP in an NPVP
    */
  case class VPIf(vp: VerbPhrase, ifc: MathExpr) extends MathExpr

  object VPIf {
    implicit def rw: RW[VPIf] = macroRW
  }

  /**
    * is ... property given by a noun.
    */
  case class IsNoun(property: NounPhrase) extends VerbPhrase // case of VerbObj

  object IsNoun {
    implicit def rw: RW[IsNoun] = macroRW
  }

  /**
    * is ... property given by an intransitive adjective.
    */
  case class IsAdj(coProperty: AdjectivalPhrase) extends VerbPhrase

  object IsAdj {
    implicit def rw: RW[IsAdj] = macroRW
  }

  /**
    * are ... property given by a transitive adjective, e.e., independent.
    */
  case class AreAdj(coProperty: AdjectivalPhrase) extends VerbPhrase

  object AreAdj {
    implicit def rw: RW[AreAdj] = macroRW
  }

  // case class VerbAdj(verb: MathExpr, adjs : Vector[MathExpr]) extends VerbPhrase

  /**
    * is ... prep ... property given by a transitive adjective
    */
  case class IsAdjPrep(adj: AdjectivalPhrase, pp: PP) extends VerbPhrase

  object IsAdjPrep {
    implicit def rw: RW[IsAdjPrep] = macroRW
  }

  /**
    * is such that ... property
    */
  case class IsSuchThat(st: SuchThat) extends VerbPhrase

  object IsSuchThat {
    implicit def rw: RW[IsSuchThat] = macroRW
  }

  /**
    * is ... property given by a prespositional phrase
    */
  case class IsPrep(pp: PP) extends VerbPhrase

  object IsPrep {
    implicit def rw: RW[IsPrep] = macroRW
  }

  case class VerbPP(verb: MathExpr, pps: Vector[MathExpr]) extends VerbPhrase

  object VerbPP {
    implicit def rw: RW[VerbPP] = macroRW
  }

  /**
    * Universally quantified sentence.
    */
  case class ForAllSP(sentence: SententialPhrase) extends SententialPhrase

  object ForAllSP {
    implicit def rw: RW[ForAllSP] = macroRW
  }

  /**
    * Existential-style quantified sentence, but also allowing at most one and precisely one.
    */
  case class ExistentialSP(sentence: SententialPhrase,
                           exists: Boolean = true,
                           unique: Boolean = false)
      extends SententialPhrase

  object ExistentialSP {
    implicit def rw: RW[ExistentialSP] = macroRW
  }

  /**
    * Negation of a sentence.
    */
  case class NegSP(sentence: SententialPhrase) extends SententialPhrase

  object NegSP {
    implicit def rw: RW[NegSP] = macroRW
  }

  /**
    * Conjunction of sentential phrases.
    */
  case class ConjSP(sentences: Vector[SententialPhrase])
      extends SententialPhrase

  object ConjSP {
    implicit def rw: RW[ConjSP] = macroRW
  }

  /**
    * Disjunction of sentential phrases.
    */
  case class DisjSP(sentences: Vector[SententialPhrase])
      extends SententialPhrase

  object DisjSP {
    implicit def rw: RW[DisjSP] = macroRW
  }

  /**
    * Sentential phrases connected by "i.e., " or "so, ".
    */
  case class ThatIsSP(sentences: Vector[SententialPhrase])
      extends SententialPhrase

  object ThatIsSP {
    implicit def rw: RW[ThatIsSP] = macroRW
  }
}

object FormalExpr {
  import MathExpr._

  case class Leaf(s: String) extends MathExpr {
    override def toString = s"Leaf($tq$s$tq)"
  }

  object Leaf {
    implicit def rw: RW[Leaf] = macroRW
  }

  case class Node(s: String, children: Vector[MathExpr])
      extends MathExpr {
    override def toString = s"Node($tq$s$tq, $children)"
  }

  object Node {
    implicit def rw: RW[Node] = macroRW
  }

  import Translator.Pattern

  import Functors._

  // import cats._

  import cats.implicits._

  type SL[A] = (S[A], Vector[A])

  // implicit val travSL = traversePair[S, Vector]

  case class Vec(vec: Vector[MathExpr]) extends MathExpr

  object Vec {
    implicit def rw: RW[Vec] = macroRW
  }

  val translator =
    Translator.Empty[Tree, MathExpr] || Pattern.partial[Tree, S] {
      case PennTrees.Leaf(s) =>
        s
    } >>> {
      case s => Leaf(s)
    } ||
      Pattern.partial[Tree, SL] {
        case PennTrees.Node(s, l) => (s, l)
      } >>> {
        case ("NP", Vector(dp: MathExpr.DP)) =>
          pprint.log(dp)
          dp
        case (s, l)                          => Node(s, l)
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

  implicit def rw: RW[Raw] = macroRW
}

sealed trait MathText

object MathText {

  import MathExpr._

  case class Assert(assertion: SententialPhrase) extends MathText

  case class Assume(assumption: SententialPhrase) extends MathText

  case class BiImplicationDefiniendumSP(definiendum: SententialPhrase)
      extends SententialPhrase

  object BiImplicationDefiniendumSP {
    implicit def rw: RW[BiImplicationDefiniendumSP] = macroRW
  }

  case class BiImplicationDefiniendum(name: String,
                                      variables: Vector[T],
                                      formula: SententialPhrase)
      extends MathExpr {
    override def toString =
      s"BiImplicationDefiniendum($tq$name$tq, $variables, $formula)"
  }

  object BiImplicationDefiniendum {
    implicit def rw: RW[BiImplicationDefiniendum] = macroRW
  }

  case class CopulaDefiniendumNP(definiendum: NounPhrase) extends NounPhrase

  object CopulaDefiniendumNP {
    implicit def rw: RW[CopulaDefiniendumNP] = macroRW
  }

  case class CopulaDefiniendum(name: String,
                               variables: Vector[T],
                               lhs: NounPhrase)
      extends MathExpr {
    override def toString = s"CopulaDefiniendum($tq$name$tq, $variables, $lhs)"
  }

  object CopulaDefiniendum {
    implicit def rw: RW[CopulaDefiniendum] = macroRW
  }

  case class BiEquationalDefinitionSP(definiendum: BiEquationalDefinitionSP,
                                      definiens: SententialPhrase)
      extends SententialPhrase

  object BiEquationalDefinitionSP {
    implicit def rw: RW[BiEquationalDefinitionSP] = macroRW
  }

  case class CopulaDefinitionSP(definiendum: CopulaDefiniendumNP,
                                definiens: NounPhrase)
      extends SententialPhrase

  object CopulaDefinitionSP {
    implicit def rw: RW[CopulaDefinitionSP] = macroRW
  }

  case class BiEquationalDefinition(definiendum: BiEquationalDefinition,
                                    definiens: SententialPhrase)
      extends SententialPhrase

  object BiEquationalDefinition {
    implicit def rw: RW[BiEquationalDefinition] = macroRW
  }

  case class CopulaDefinition(definiendum: CopulaDefiniendum,
                              definiens: NounPhrase)
      extends SententialPhrase

  object CopulaDefinition {
    implicit def rw: RW[CopulaDefinition] = macroRW
  }

  case class VariableType(variables: Vector[NounPhrase], typ: NounPhrase)

  object VariableType {
    implicit def rw: RW[VariableType] = macroRW
  }
}
