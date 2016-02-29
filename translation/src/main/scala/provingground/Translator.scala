package provingground

import scala.language.implicitConversions

import scala.util.Try
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
 * scala> import provingground._
 * import provingground._
 *
 * scala> import OptMapLift._
 * import OptMapLift._
 *
 * scala> class A
 * defined class A
 *
 * scala> case class B(x: A, y: A) extends A
 * defined class B
 *
 * scala> case class C(x: A, y: A) extends A
 * defined class C
 *
 * scala> case object D extends A
 * defined object D
 *
 * scala> val tt = Translator.Simple((x: A) => Some(x))
 * tt: provingground.Translator.Simple[A,A] = <function1>
 *
 * scala> val t = tt.<<>>(B.unapply _ , (C.apply _).tupled)
 * t: provingground.Translator.OrElse[A,A] = <function1>
 *
 * scala> t (B(D, D))
 * res1: Option[A] = Some(C(D,D))
 *
 *
 * }}}
 */
trait Translator[I, O] extends (I => Option[O]) { self =>
  /**
   * the abstract method translating using an in general different translator on sub-inputs
   * @param rec The translator used on sub-inputs
   */
  def recTranslate(rec: => Translator[I, O]): I => Option[O]

  /**
   * returns the optional result of translation.
   */
  def apply(inp: I) = recTranslate(self)(inp)

  /**
   * OrElse combinator, tries other translator if the first fails.
   */
  def |(that: Translator[I, O]) = Translator.OrElse(self, that)

  /**
   * OrElse combinator, tries other translator first, then this one.
   * Other translator must be preprended
   */
  def |:(that: Translator[I, O]) = Translator.OrElse(that, self)

  def ||[X >: I, Y <: O](that: X => Option[Y]) = |(Translator.Simple[I, O](that))

  /**
   * mixin translator obtained by optionally pattern matching and optionally joining.
   * this is tried if the present one fails.
   */
  def <|>(split: I => Option[I], join: O => Option[O]) =
    |(Translator.Junction(split, join))

  /**
   * mixin translator obtained by optionally pattern matching and optionally joining.
   * the new  translator is tried first.
   */
  def <<|>>:(splitJoin: (I => Option[I], O => Option[O])) = <|>:(splitJoin._1, splitJoin._2)

  /**
   * mixin translator obtained by optionally splitting and optionally joining.
   * this is tried if the present one fails.
   */
  def orElse[J <: I, XI, XO](
    split: J => Option[XI], join: XO => Option[O]
  )(implicit oml: OptMapLift[I, O, XI, XO]) =
    |(Translator.MultiJunction[I, O, J, XI, XO](split, join))

  /**
   * mixin translator after splitting and joining with _different_ input and output  types;
   * lift of a specified translator is applied to the result of splitting before joining.
   */
  def orElse[J <: I, II, OO, XI, XO](
    split: J => Option[XI], subTranslate: II => Option[OO], join: XO => Option[O]
  )(implicit oml: OptMapLift[II, OO, XI, XO]) =
    self | Translator.Simple(Translator.HybridJunction(split, subTranslate, join))

  /**
   * mixin translator obtained by optionally splitting and optionally joining.
   * this is tried if the present one fails.
   */
  def <>[J <: I, XI, XO](
    split: J => Option[XI], join: XO => Option[O]
  )(implicit oml: OptMapLift[I, O, XI, XO]) = orElse(split, join)

  /**
   * mixin translator after splitting and joining with _different_ input and output  types;
   * lift of a specified translator is applied to the result of splitting before joining.
   */
  def <>[J <: I, II, OO, XI, XO](
    split: J => Option[XI], subTranslate: II => Option[OO], join: XO => Option[O]
  )(implicit oml: OptMapLift[II, OO, XI, XO]) =
    orElse(split, subTranslate, join)

  /**
   * mixin translator obtained by optionally pattern matching and optionally joining.
   * the new translator (which is a prefix) is tried first.
   */
  def <|>:(split: I => Option[I], join: O => Option[O]) =
    |:(Translator.Junction(split, join))

  /**
   * mixin translator obtained by optionally splitting and optionally joining.
   * the new translator is tried first.
   */
  def elseOr[J <: I, XI, XO](
    split: J => Option[XI], join: XO => Option[O]
  )(implicit oml: OptMapLift[I, O, XI, XO]) =
    |:(Translator.MultiJunction[I, O, J, XI, XO](split, join))

  /**
   * mixin translator after splitting and joining with _different_ input and output  types;
   * lift of a specified translator is applied to the result of splitting before joining.
   */
  def elseOr[J <: I, II, OO, XI, XO](
    split: J => Option[XI], subTranslate: II => Option[OO], join: XO => Option[O]
  )(implicit oml: OptMapLift[II, OO, XI, XO]) =
    |:(Translator.Simple(
      Translator.HybridJunction(split, subTranslate, join)
    ))

  /**
   * mixin translator after splitting and joining with _different_ input and output  types;
   * lift of a specified translator is applied to the result of splitting before joining.
   */
  def <<>>[J <: I, II, OO, XI, XO](
    split: J => Option[XI], subTranslate: II => Option[OO], join: XO => Option[O]
  )(implicit oml: OptMapLift[II, OO, XI, XO]) =
    elseOr(split, subTranslate, join)

  /**
   * mixin translator obtained by optionally splitting and optionally joining.
   * the new translator is tried first.
   */
  def <<>>[J <: I, XI, XO](
    split: J => Option[XI], join: XO => Option[O]
  )(implicit oml: OptMapLift[I, O, XI, XO]) = elseOr(split, join)

  /**
   * Restricts domain and expands codomain by treating this as simple a function I => Option[O]
   */
  def as[X <: I, Y >: O] = Translator.Simple[X, Y](self)
}

object Translator {
  /**
   * Empty translator, always fails
   */
  case class Empty[I, O]() extends Translator[I, O] {
    def recTranslate(rec: => Translator[I, O]) = (_: I) => None
  }

  /**
   * Simple translator given by a function. Ignores sub-translator
   */
  case class Simple[I, O](translate: I => Option[O]) extends Translator[I, O] {
    def recTranslate(rec: => Translator[I, O]) = translate
  }

  /**
   * Tries the first translator at top level, then the second. Is recursive.
   */
  case class OrElse[I, O](first: Translator[I, O], second: Translator[I, O]) extends Translator[I, O] {
    def recTranslate(rec: => Translator[I, O]) =
      (inp: I) => first.recTranslate(rec)(inp) orElse second.recTranslate(rec)(inp)
  }

  /**
   * Translator built from pattern matching and joining, but without any actual splitting.
   * In case of a match, applies sub-translator to result.
   */
  case class Junction[I, O](split: I => Option[I], join: O => Option[O]) extends Translator[I, O] {
    def recTranslate(rec: => Translator[I, O]) =
      (inp: I) => split(inp) flatMap ((x: I) => rec(x).flatMap(join(_)))
  }

  /**
   * The main recursive translator:
   *
   * @param split matches and optionally splits to a type XI derived from I, possibly starting with a sub-type of I.
   * @param join optionally combined from a type XO built from O, possibly starting with a sub-type of O.
   *
   * inference of I and O is difficult in this case, so should generally not be used directly.
   * Instead when combining translations I and J are determined by the base, and the rest of the
   * type parameters by the split and join.
   */
  case class MultiJunction[I, O, J <: I, XI, XO](
    split: J => Option[XI], join: XO => Option[O]
  )(implicit oml: OptMapLift[I, O, XI, XO]) extends Translator[I, O] {

    def recTranslate(rec: => Translator[I, O]) = {
      (inp: I) =>
        Try(
          split(inp.asInstanceOf[J]) flatMap ((x: XI) => oml.lift(rec.apply)(x).flatMap(join(_)))
        ).toOption.flatten
    }
  }

  case class HybridJunction[I, O, J <: I, II, OO, XI, XO](
    split: J => Option[XI], subTranslate: II => Option[OO], join: XO => Option[O]
  )(implicit oml: OptMapLift[II, OO, XI, XO]) extends (I => Option[O]) {

    def apply(inp: I) =
      Try(
        split(inp.asInstanceOf[J]) flatMap ((x: XI) => oml.lift(subTranslate)(x).flatMap(join(_)))
      ).toOption.flatten
  }

}

/**
 * Gives lift from a map I => Option[O] to a map X => Option[Y].
 * Generally constructed using combinators, starting with the identity.
 */
trait OptMapLift[I, O, XI, XO] {
  def lift(optMap: I => Option[O]): XI => Option[XO]
}

object OptMapLift {
  implicit def toOptMap[I, O](fn: I => O): I => Option[O] = (x: I) => Some(fn(x))

  /**
   * Trivial lifting by the identity
   */
  implicit def idLift[I, O] = new OptMapLift[I, O, I, O] {
    def lift(optMap: I => Option[O]) = optMap
  }

  /**
   * lift by restricting the domain.
   */
  implicit def restrictLift[I, O, J <: I] = new OptMapLift[I, O, J, O] {
    def lift(optMap: I => Option[O]) = (inp: J) => optMap(inp)
  }

  /**
   * Lift to a restricted codomain, by attempting to cast and returning None on failing.
   */
  implicit def filterLift[I, O, L <: O] = new OptMapLift[I, O, I, L] {
    def lift(optMap: I => Option[O]) =
      (inp: I) => optMap(inp) flatMap ((x: O) => Try(x.asInstanceOf[L]).toOption)
  }

  /**
   * Given a lift to XI and XO, lift to maps between corresponding lists.
   */
  implicit def listLift[I, O, XI, XO](implicit lft: OptMapLift[I, O, XI, XO]) = new OptMapLift[I, O, List[XI], List[XO]] {
    def lift(optMap: I => Option[O]) =
      {
        def lstMap(inp: List[XI]): Option[List[XO]] = inp match {
          case List() => Some(List())
          case x :: ys =>
            for (a <- lft.lift(optMap)(x); bs <- lstMap(ys)) yield a :: bs
        }
        lstMap _
      }
  }

  /**
   * Lift to tuple given lift to individual components
   */
  implicit def tuple2Lift[I, O, XI1, XO1, XI2, XO2](
    implicit
    lft1: OptMapLift[I, O, XI1, XO1], lft2: OptMapLift[I, O, XI2, XO2]
  ) =
    new OptMapLift[I, O, (XI1, XI2), (XO1, XO2)] {
      def lift(optMap: I => Option[O]) = {
        case (x, y) =>
          for (a <- lft1.lift(optMap)(x); b <- lft2.lift(optMap)(y)) yield (a, b)
      }
    }

  /**
   * Lift to tuple given lift to individual components
   */
  implicit def tuple3Lift[I, O, XI1, XO1, XI2, XO2, XI3, XO3](
    implicit
    lft1: OptMapLift[I, O, XI1, XO1], lft2: OptMapLift[I, O, XI2, XO2], lft3: OptMapLift[I, O, XI3, XO3]
  ) =
    new OptMapLift[I, O, (XI1, XI2, XI3), (XO1, XO2, XO3)] {
      def lift(optMap: I => Option[O]) = {
        case (x, y, z) =>
          for (a <- lft1.lift(optMap)(x); b <- lft2.lift(optMap)(y); c <- lft3.lift(optMap)(z)) yield (a, b, c)
      }
    }

  /**
   * Lift to tuple given lift to individual components
   */
  implicit def tuple4Lift[I, O, XI1, XO1, XI2, XO2, XI3, XO3, XI4, XO4](
    implicit
    lft1: OptMapLift[I, O, XI1, XO1], lft2: OptMapLift[I, O, XI2, XO2],
    lft3: OptMapLift[I, O, XI3, XO3], lft4: OptMapLift[I, O, XI4, XO4]
  ) =
    new OptMapLift[I, O, (XI1, XI2, XI3, XI4), (XO1, XO2, XO3, XO4)] {
      def lift(optMap: I => Option[O]) = {
        case (x, y, z, w) =>
          for (
            a <- lft1.lift(optMap)(x); b <- lft2.lift(optMap)(y);
            c <- lft3.lift(optMap)(z); d <- lft4.lift(optMap)(w)
          ) yield (a, b, c, d)
      }
    }
}
