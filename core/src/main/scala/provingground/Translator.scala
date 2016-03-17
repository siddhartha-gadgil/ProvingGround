package provingground

import scala.language.higherKinds

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
  case class Simple[I, O](translate : I => Option[O]) extends Translator[I, O]{
  
    def recTranslate(leafMap: =>(I => Option[O])) = translate
  }
  
  def connect[I, O, S](pattern: I => Option[S], literal: S => Option[O]) =
    Simple((inp: I) => pattern(inp) flatMap literal)
  
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
  
  case class Junction[I, O, X[_]: Functor : OptNat](split: I => Option[X[I]], build: X[O] => Option[O]) extends Translator[I, O]{
    def flip = implicitly[OptNat[X]].optF[O](_)
    
    def recTranslate(leafMap: => (I => Option[O])) = {
      def connect(xi: X[I]) = flip(implicitly[Functor[X]].map(xi)(leafMap)) 
      (inp: I) => split(inp) flatMap (connect) flatMap (build)
    }
  }
  
}