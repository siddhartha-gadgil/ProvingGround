package provingGround

import provingGround.NlpProse._
import provingGround.HoTT._
import provingGround.TheoryTypes.{Term => TheoryTerm, _} 


import scala.language.implicitConversions

/** Parses prose to HoTT
 *  Gives context-free expressions, not actual typed terms.
 *  Warning: Should handle conjunctions carefully
 */
object NLPHoTT{
  val parse: ProseTree =>  TheoryTerm = {
    case ProseTree(root, List()) => TermSym(root.word)
    
    case Cop(first, second) => Is(parse(first), parse(second))
    
    case Conj(conj, _, sub, rest) => Conjunct(conj, parse(sub), parse(rest))
    
    case Modifier(_, sub, rest) => Apply(parse(sub), parse(rest))
    
    case Argument(_, sub, rest) => Apply(parse(rest), parse(sub))
    
    case Parataxis(_, sub, rest) => parse(rest) \\ parse(sub)
    
    case t => TermSym(t)
  }
	
}
