package provingground.nlp
import provingground._

import translation._

import NlpProse._
//import provingground.HoTT._
import provingground.nlp.TheoryTypes.{
  Term => TheoryTerm,
  Apply => TheoryApply,
  _
}

//import scala.language.implicitConversions

/**
  * Parses prose to HoTT
  *  Gives context-free expressions, not actual typed terms.
  *  Warning: Should handle conjunctions carefully
  */
@deprecated("use TreePatterns and MathExpr", "0.1")
object NLPHoTT {
  val parse: ProseTree => TheoryTerm = {
    case ProseTree(root, List()) => TermSym(root.word)

    case Cop(first, second) => Is(parse(first), parse(second))

    case Conj(conj, _, sub, rest) => Conjunct(conj, parse(sub), parse(rest))

    case Modifier(_, sub, rest) => TheoryApply(parse(sub), parse(rest))

    case Argument(_, sub, rest) => TheoryApply(parse(rest), parse(sub))

    case Parataxis(_, sub, rest) => parse(rest) \\ parse(sub)

    case t => TermSym(t)
  }
}
