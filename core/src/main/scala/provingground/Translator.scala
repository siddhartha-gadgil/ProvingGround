package provingground

import scala.language.higherKinds
import scala.util.Try

import cats.implicits._

import cats._

trait Translator[I, O] extends (I => Option[O]) { self =>
  def apply(inp: I) = recTranslate(self)(inp)

  def recTranslate(leafMap: => (I => Option[O])): I => Option[O]

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

  def map[X](fn: O => X, ufn: X => O) = Translator.Mapped(self, fn, ufn)
}

object Translator {

  /**
    * non-recursive translation determined by a given optional function
    */
  case class Simple[I, O](translate: I => Option[O]) extends Translator[I, O] {

    def recTranslate(leafMap: => (I => Option[O])) = translate
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
    def recTranslate(leafMap: => (I => Option[O])) = (_: I) => None
  }

  /**
    * Tries the first translator at top level, then the second. Is recursive.
    */
  case class OrElse[I, O](first: Translator[I, O], second: Translator[I, O])
      extends Translator[I, O] {
    def recTranslate(leafMap: => (I => Option[O])) =
      (inp: I) =>
        first.recTranslate(leafMap)(inp) orElse second.recTranslate(leafMap)(
            inp)
  }

  /**
    * A junction given by splitting optionally to a given shape, and building from the same shape.
    *
    * The shape is functorial, typically made of tuples and lists, and Option gives a natural transformation.
    * These allow applying the recursive translator on the components.
    */
  case class Junction[I, O, X[_]: Traverse](
      split: I => Option[X[I]], build: X[O] => Option[O])
      extends Translator[I, O] {
    def flip : X[Option[O]] => Option[X[O]] = ???

    def recTranslate(leafMap: => (I => Option[O])) = {
      def connect(xi: X[I]) = flip(implicitly[Functor[X]].map(xi)(leafMap))
      (inp: I) =>
        split(inp) flatMap (connect) flatMap (build)
    }
  }

  case class Mapped[I, O, X](trans: Translator[I, O], fn: O => X, ufn: X => O)
      extends Translator[I, X] {
    def recTranslate(leafMap: => (I => Option[X])): I => Option[X] =
      (x) => trans.recTranslate((a: I) => leafMap(a) map (ufn))(x) map (fn)
  }

  /**
    * The splitting part of a junction, pattern matching and splitting to a given shape.
    * Crucially, the shape X[_] is determined, so junctions can be built from this, after possibly mapping.
    */
  class Pattern[I, X[_]: Traverse](split: I => Option[X[I]]) {
    def unapply(x: I): Option[X[I]] = split(x)

    def map[J](f: I => I) = {
      val lift = implicitly[Functor[X]].lift(f)
      Pattern((inp: I) => unapply(inp) map lift)
    }

    def join[O](build: X[O] => Option[O]) = Junction(unapply, build)

    def joinStrict[O](build: X[O] => O) = join((x: X[O]) => Some(build(x)))

    def >>[O](build: X[O] => Option[O]) = join(build)

    def >>>[O](build: X[O] => O) = joinStrict(build)

    def ||(that: Pattern[I, X]) = Pattern.OrElse(this, that)
  }

  object Pattern {
    def apply[I, X[_]: Traverse](split: I => Option[X[I]]) =
      new Pattern(split)

    def partial[I, X[_]: Traverse](split: PartialFunction[I, X[I]]) =
      Pattern(split.lift)

    // To allow for inheritance so we can use case objects.
    class Partial[I, X[_]: Traverse](split: PartialFunction[I, X[I]])
        extends Pattern(split.lift)

    case class OrElse[I, X[_]: Traverse](
        first: Pattern[I, X], second: Pattern[I, X])
        extends Pattern[I, X](
            (x: I) => first.unapply(x) orElse second.unapply(x)
        )

    def filter[I](p: I => Boolean) =
      Pattern[I, Id] { (x: I) =>
        if (p(x)) Some(x) else None
      }

    /**
      * Builds a splitter from a word of a given shape, and a map that matches and returns the image of an element.
      * This is problematic if lists should be returned.
      */
    def fromMatcher[I, X[_]: Traverse, S](
        matcher: I => Option[Map[S, I]], varword: X[S]) = {
      Pattern(
          (inp: I) => matcher(inp) map (implicitly[Functor[X]].lift(_)(varword))
      )
    }

    /**
      * Given shape and Input type, builds a junction from a split whose output is _a priori_ of the wrong type.
      * The shape X[_] must be specified.
      */
    def cast[I, X[_]: Traverse](split: I => Option[Any]) =
      Pattern[I, X]((inp: I) =>
            split(inp) flatMap ((xi) => Try(xi.asInstanceOf[X[I]]).toOption))
  }

  /**
    * A word fixing the shape of patterns to which we match. Should work fine for tuples, but problematic with Lists returned.
    */
  case class VarWord[X[_]: Traverse, S](word: X[S]) {
    def apply[I](matcher: I => Option[Map[S, I]]) =
      Pattern.fromMatcher(matcher, word)
  }
}
