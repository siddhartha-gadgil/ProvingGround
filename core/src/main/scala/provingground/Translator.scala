package provingground

import scala.language.higherKinds
import scala.util.Try

trait Translator[I, O] extends  (I => Option[O]){self =>
  def apply(inp: I) = recTranslate(self)(inp)

  def recTranslate(leafMap: =>(I => Option[O])): I => Option[O]
  
  /**
   * OrElse combinator, tries other translator if the first fails.
   */
  def orElse(that: Translator[I, O]) = Translator.OrElse(this, that)
  
  /**
   * OrElse combinator, tries other translator first, then this one.
   * Other translator must be preprended
   */
  def elseOr(that: Translator[I, O]) = Translator.OrElse(that, this)

  /**
   * OrElse combinator, tries other translator if the first fails.
   */
  def ||(that: Translator[I, O]) = Translator.OrElse(self, that)

  /**
   * OrElse combinator, tries other translator first, then this one.
   * Other translator must be preprended
   */
  def ||:(that: Translator[I, O]) = Translator.OrElse(that, self)


}


object Translator{
  /**
   * non-recursive translation determined by a given optional function
   */
  case class Simple[I, O](translate : I => Option[O]) extends Translator[I, O]{
  
    def recTranslate(leafMap: =>(I => Option[O])) = translate
  }
  
  /**
   * non-recursive translation by combining a matching pattern with a simple builder (usually a literal)
   */
  def connect[I, O, S](pattern: I => Option[S], literal: S => Option[O]) =
    Simple((inp: I) => pattern(inp) flatMap literal)
  
  /**
   * empty translator, matching nothing; 
   * only information is the type parameters, so may be good starting point.  
   */
  case class Empty[I, O]() extends Translator[I, O] {
    def recTranslate(leafMap: =>(I => Option[O])) = (_: I) => None
  }

  /**
   * Tries the first translator at top level, then the second. Is recursive.
   */
  case class OrElse[I, O](first: Translator[I, O], second: Translator[I, O]) extends Translator[I, O] {
    def recTranslate(leafMap: =>(I => Option[O])) =
      (inp: I) => first.recTranslate(leafMap)(inp) orElse second.recTranslate(leafMap)(inp)
  }
  
  /**
   * A junction given by splitting optionally to a given shape, and building from the same shape.
   * 
   * The shape is functorial, typically made of tuples and lists, and Option gives a natural transformation.
   * These allow applying the recursive translator on the components.
   */
  case class Junction[I, O, X[_]: Functor : OptNat](split: I => Option[X[I]], build: X[O] => Option[O]) extends Translator[I, O]{
    def flip = implicitly[OptNat[X]].optF[O](_)
    
    def recTranslate(leafMap: => (I => Option[O])) = {
      def connect(xi: X[I]) = flip(implicitly[Functor[X]].map(xi)(leafMap)) 
      (inp: I) => split(inp) flatMap (connect) flatMap (build)
    }
  }
  
  /**
   * The splitting part of a junction, pattern matching and splitting to a given shape.
   * Crucially, the shape X[_] is determined, so junctions can be built from this, after possibly mapping.
   */
  case class MatchSplit[I, X[_]: Functor : OptNat](split: I => Option[X[I]]){
    def map[J](f: I => I) = {
      val lift = (xi: X[I]) => Functor.liftMap(xi, f)
      MatchSplit((inp: I) => split(inp) map lift)
    }
    
    def join[O](build: X[O] => Option[O]) = Junction(split, build)
  }
  
  
  object MatchSplit{
    /**
     * Builds a splitter from a word of a given shape, and a map that matches and returns the image of an element.
     * This is problematic if lists should be returned.
     */
    def fromMatcher[I, X[_]: Functor : OptNat, S](matcher: I => Option[Map[S, I]], varword: X[S]) = {
      MatchSplit( 
        (inp: I) => 
          matcher(inp) map (Functor.liftMap(varword, _))
        )
    }
    
    /**
     * Given shape and Input type, builds a junction from a split whose output is _a priori_ of the wrong type.
     * The shape X[_] must be specified.
     */
    def cast[I, X[_]: Functor : OptNat](split: I => Option[Any]) = 
      MatchSplit[I, X]((inp: I) => split(inp) flatMap ((xi) => Try(xi.asInstanceOf[X[I]]).toOption)) 
  }
  
  /**
   * A word fixing the shape of patterns to which we match. Should work fine for tuples, but problematic with Lists returned.
   */
  case class VarWord[X[_]: Functor : OptNat, S](word: X[S]){
    def apply[I](matcher: I => Option[Map[S, I]]) = MatchSplit.fromMatcher(matcher, word)
  }
}