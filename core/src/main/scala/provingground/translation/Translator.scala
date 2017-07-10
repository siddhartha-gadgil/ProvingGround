package provingground.translation

import scala.language.higherKinds
import scala.util.Try

import cats.implicits._

import cats._

/**
  * Translator from an input I to an output O, designed to be built recursively.
  *
  *
  * ==Translation by Splitting and joining==
  *
  * The most import class of translators are constructed from
  *
  * * a split map I => Option[X], where X is obtained from I by taking Tuples, Lists and sub-types
  *
  * * a join map Y => Option[O], where Y is obtained from O structurally, in the same way as X is obtained from I,
  * e.g. X = (I, I) and Y = (O, O).
  *
  * However X and Y may involve taking sub-types independently of each other.
  *
  * Such a translator splits an input, recursively translates X to Y and combines the result with the join (all steps work optionally).
  *
  * ==Combinations, Basic Translators==
  *
  * Translators are built by combining others by OrElse, starting with basic translators specified by a function I => Option[O].
  * One can instead start with an empty translator.
  *
  * Note that we can restrict the domain or extend the codomain of a translator by
  * just using it as a function I => Option[O]. However this should be done after making all OrElse combinations, as the
  * wrapped translator does not combine recursively.
  *
  * A typical example of usage is as below
  * {{{
scala> import provingground._
import provingground._

scala> import translation._, Translator._
import translation._
import Translator._

scala> import cats._, cats.implicits._
import cats._
import cats.implicits._

scala> class A
defined class A

scala> case class B(x: A, y: A) extends A
defined class B

scala> case class C(x: A, y: A) extends A
defined class C

scala> case object D extends A
defined object D

scala> import Functors._
import Functors._

scala> val Bpat = Pattern.partial[A, II]{case B(x, y) => (x, y)}
Bpat: provingground.translation.Translator.Pattern[A,provingground.translation.Functors.II] = provingground.translation.Translator$Pattern@5ba695d

scala> val Dpat = Pattern.partial[A, Un]{case D => ()}
Dpat: provingground.translation.Translator.Pattern[A,provingground.translation.Functors.Un] = provingground.translation.Translator$Pattern@2f5c9798

scala> case object E extends A
defined object E

scala> val trans = Translator.Empty[A, A] || Bpat >>> {case (x, y) => C(x,y)} || Dpat >>> {case _ => E}
trans: provingground.translation.Translator.OrElse[A,A] = <function1>

scala> trans(B(B(D, D), B(B(D, D), D)))
res0: Option[A] = Some(C(C(E,E),C(C(E,E),E)))

scala> trans(C(D, D))
res1: Option[A] = None
}}}
  */
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

/**
  * General functorial framework for translation.
  *
  * Translators with `I` the input type and `O` the output type are primarily built from [[Junction]]s
  *   - a ``pattern`` which map `I => Option[X[O]]`, with  `X[_]` a functor with traverse, e.g. a tuple or a vector.
  *   - a `builder` `O => Option[I]`
  * to avoid having to specify types too many types, traits [[Pattern]] and [[Builder]] are defined.
  *
  * We also have simpler translators for literals and also wrapping translators for a component type.
  */
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
  case class Junction[I, O, X[_]: Traverse](split: I => Option[X[I]],
                                            build: X[O] => Option[O])
      extends Translator[I, O] {
    def flip: X[Option[O]] => Option[X[O]] = (xo) => xo.sequence
    def recTranslate(leafMap: => (I => Option[O])) = {
      def connect(xi: X[I]) = flip(implicitly[Functor[X]].map(xi)(leafMap))
      (inp: I) =>
        split(inp) flatMap (connect) flatMap (build)
    }
  }

  /**
    * like a [[Junction]] but tries several cases.
    */
  case class PolyJunction[I, O, X[_]: Traverse](
      polySplit: I => Option[Vector[X[I]]],
      build: X[O] => Option[O])
      extends Translator[I, O] {
    def flip: X[Option[O]] => Option[X[O]] = (xo) => xo.sequence
    def recTranslate(leafMap: => (I => Option[O])) = {
      def connect(xi: X[I]) = flip(implicitly[Functor[X]].map(xi)(leafMap))
      (inp: I) =>
        (polySplit(inp) flatMap { (v) =>
          (v flatMap (xi => (connect(xi)) flatMap (build))).headOption
        })
    }
  }

  /**
    * mapped translator
    */
  case class Mapped[I, O, X](trans: Translator[I, O], fn: O => X, ufn: X => O)
      extends Translator[I, X] {
    def recTranslate(leafMap: => (I => Option[X])): I => Option[X] =
      (x) => trans.recTranslate((a: I) => leafMap(a) map (ufn))(x) map (fn)
  }

  /**
    * The building part of a junction, pattern matching and splitting to a given shape.
    * Crucially, the shape X[_] is determined, so junctions can be built from this, after possibly mapping.
    */
  case class Builder[O, X[_]: Traverse](build: X[O] => Option[O]) {
    def join[I](split: I => Option[X[I]]) = Junction(split, build)

    def on[I](sp: PartialFunction[I, X[I]]) = Junction(sp.lift, build)

    def onOpt[I, Y[_]](spl: I => Option[Y[I]])(implicit eq: Equiv[X, Y]) =
      join((a: I) => spl(a) map (eq.inv))
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

    /**
      * pattern from a partial function
      */
    def partial[I, X[_]: Traverse](split: PartialFunction[I, X[I]]) =
      Pattern(split.lift)

    /**
      * pattern as a class from a partial function - defined as a class to allow inheritance
      */
    class Partial[I, X[_]: Traverse](split: PartialFunction[I, X[I]])
        extends Pattern(split.lift)

    /**
      * Tries the first translator at top level, then the second. Is recursive.
      */
    case class OrElse[I, X[_]: Traverse](first: Pattern[I, X],
                                         second: Pattern[I, X])
        extends Pattern[I, X](
          (x: I) => first.unapply(x) orElse second.unapply(x)
        )

    /**
      * filtered pattern
      */
    def filter[I](p: I => Boolean) =
      Pattern[I, Id] { (x: I) =>
        if (p(x)) Some(x) else None
      }

    import Functors._

    /**
      * pattern by checking condition, returns optional Unit
      */
    def check[I](p: I => Boolean) =
      Pattern[I, Un] { (x: I) =>
        if (p(x)) Some(()) else None
      }

    /**
      * Builds a splitter from a word of a given shape, and a map that matches and returns the image of an element.
      * This is problematic if lists should be returned.
      */
    def fromMatcher[I, X[_]: Traverse, S](matcher: I => Option[Map[S, I]],
                                          varword: X[S]) = {
      Pattern(
        (inp: I) => matcher(inp) map (implicitly[Functor[X]].lift(_)(varword))
      )
    }

    /**
      * like a [[Pattern]] but with multiple matches possible
      */
    class PolyPattern[I, X[_]: Traverse](split: I => Option[Vector[X[I]]]) {
      def unapply(x: I): Option[Vector[X[I]]] = split(x)

      def join[O](build: X[O] => Option[O]) = PolyJunction(unapply, build)

      def joinStrict[O](build: X[O] => O) = join((x: X[O]) => Some(build(x)))

      def >>[O](build: X[O] => Option[O]) = join(build)

      def >>>[O](build: X[O] => O) = joinStrict(build)
    }

    object PolyPattern {
      def apply[I, X[_]: Traverse](split: I => Option[Vector[X[I]]]) =
        new PolyPattern(split)

      def partial[I, X[_]: Traverse](split: PartialFunction[I, Vector[X[I]]]) =
        PolyPattern(split.lift)

// To allow for inheritance so we can use case objects.
      class Partial[I, X[_]: Traverse](split: PartialFunction[I, Vector[X[I]]])
          extends PolyPattern(split.lift)
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

/**
  * inclusion of functors
  */
trait Inclusion[X[_], Y[_]] {
  def incl[I]: X[I] => Y[I]
}

object Inclusion {
  implicit def subtypIncl[X[_], Y[_] >: X[_]]: Inclusion[X, Y] =
    new Inclusion[X, Y] {
      def incl[I] = x => x
    }

  implicit def pairInclusion[X1[_], Y1[_], X2[_], Y2[_]](
      implicit incl1: Inclusion[X1, Y1],
      incl2: Inclusion[X2, Y2]) =
    new Inclusion[({ type X[A] = (X1[A], X2[A]) })#X,
                  ({ type Y[A] = (Y1[A], Y2[A]) })#Y] {
      def incl[I] = { case (x1, x2) => (incl1.incl(x1), incl2.incl(x2)) }
    }
}

/**
  * restriction of cone on functors
  */
trait ConeRestriction[X[_], Y[_], G[_]] {
  def restrict[I]: G[Y[I]] => G[X[I]]
}

/**
  * restriction on optional functor
  */
trait OptRestriction[X[_], Y[_]] extends ConeRestriction[X, Y, Option]

import scala.util.Try

object OptRestriction {
  implicit def subtypRestriction[X[_], Y[_] >: X[_]]: OptRestriction[X, Y] =
    new OptRestriction[X, Y] {
      def restrict[I] = y => Try(y.asInstanceOf[X[I]]).toOption
    }

  implicit def pairRestriction[X1[_], Y1[_], X2[_], Y2[_], G[_]](
      implicit rest1: OptRestriction[X1, Y1],
      rest2: OptRestriction[X2, Y2]) =
    new OptRestriction[({ type X[A] = (X1[A], X2[A]) })#X,
                       ({ type Y[A] = (Y1[A], Y2[A]) })#Y] {
      def restrict[I] = _.flatMap {
        case (y1, y2) =>
          for (x1 <- rest1.restrict(Some(y1)); x2 <- rest2.restrict(Some(y2)))
            yield (x1, x2)
      }
    }
}

/**
  * subtype relation between functors, giving inclusion and optional restriction,
  * explicit, not depending on scala type checking
  */
trait SubType[X[_], Y[_]] extends Inclusion[X, Y] with OptRestriction[X, Y]

/**
  * context dependent version of [[Translator]]
  */
trait ContextTranslator[I, O, X[_], Ctx[_, _]]
    extends (Ctx[I, O] => X[I] => Option[X[O]]) { self =>
  def apply(ctx: Ctx[I, O]) = (inp: X[I]) => recTranslate(self)(ctx)(inp)

  def recTranslate(leafMap: => (Ctx[I, O] => X[I] => Option[X[O]]))
    : Ctx[I, O] => X[I] => Option[X[O]]

  import ContextTranslator._

  def ||(that: ContextTranslator[I, O, X, Ctx]) = OrElse(this, that)

  def addJunction[Y[_], Z[_]: Traverse](
      split: Ctx[I, O] => X[I] => Option[Y[I]],
      build: Ctx[I, O] => Y[O] => Option[X[O]])(
      implicit incl: Inclusion[Y, ({ type W[A] = Z[X[A]] })#W],
      rest: OptRestriction[Y, ({ type W[A]     = Z[X[A]] })#W]) = {
    val that = Junction[I, O, X, Ctx, Y, Z](split, build)
    this || that
  }

  def addJunction1[Y[_], Z[_]: Traverse](split: PartialFunction[X[I], Y[I]],
                                         build: PartialFunction[Y[O], X[O]])(
      implicit incl: Inclusion[Y, ({ type W[A] = Z[X[A]] })#W],
      rest: OptRestriction[Y, ({ type W[A]     = Z[X[A]] })#W]) = {
    addJunction[Y, Z]((_) => split.lift, (_) => build.lift)
  }
}

object ContextTranslator {

  /**
    * empty translator, matching nothing;
    * only information is the type parameters, so may be good starting point.
    */
  case class Empty[I, O, X[_], Ctx[_, _]]()
      extends ContextTranslator[I, O, X, Ctx] {
    def recTranslate(leafMap: => (Ctx[I, O] => X[I] => Option[X[O]])) =
      (ctx: Ctx[I, O]) => (x: X[I]) => None
  }

  /**
    * Tries the first translator at top level, then the second. Is recursive.
    */
  case class OrElse[I, O, X[_], Ctx[_, _]](
      first: ContextTranslator[I, O, X, Ctx],
      second: ContextTranslator[I, O, X, Ctx])
      extends ContextTranslator[I, O, X, Ctx] {
    def recTranslate(leafMap: => (Ctx[I, O] => X[I] => Option[X[O]])) =
      (ctx: Ctx[I, O]) =>
        (inp: X[I]) =>
          first.recTranslate(leafMap)(ctx)(inp) orElse second.recTranslate(
            leafMap)(ctx)(inp)
  }

  /**
    * A junction given by splitting optionally to a given shape, and building from the same shape.
    *
    * The shape is functorial, typically made of tuples and lists, and Option gives a natural transformation.
    * These allow applying the recursive translator on the components.
    */
  case class Junction[I, O, X[_], Ctx[_, _], Y[_], Z[_]: Traverse](
      split: Ctx[I, O] => X[I] => Option[Y[I]],
      build: Ctx[I, O] => Y[O] => Option[X[O]])(
      implicit incl: Inclusion[Y, ({ type W[A] = Z[X[A]] })#W],
      rest: OptRestriction[Y, ({ type W[A]     = Z[X[A]] })#W])
      extends ContextTranslator[I, O, X, Ctx] {
    def flip: Z[Option[X[O]]] => Option[Z[X[O]]] = (zo) => zo.sequence
    def recTranslate(leafMap: => (Ctx[I, O] => X[I] => Option[X[O]])) = {
      def connect(ctx: Ctx[I, O])(zi: Z[X[I]]) =
        flip(implicitly[Functor[Z]].map(zi)(leafMap(ctx)))
      (ctx: Ctx[I, O]) => (inp: X[I]) =>
        rest.restrict(split(ctx)(inp) map (incl.incl) flatMap (connect(ctx))) flatMap
          (build(ctx))
    }
  }
}
