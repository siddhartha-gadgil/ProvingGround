package provingground

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
 */
trait Translator[I, O]  extends (I => Option[O]){self =>
  /**
   * the abstract method translating using an in general different translator on sub-inputs
   * @param rec The translator used on sub-inputs
   */
  def recTranslate(rec: => Translator[I, O]): I => Option[O]

  /**
   * returns the optional result of translation.
   */
  def apply(inp: I) = recTranslate(self)(inp)

  def ||(that: Translator[I, O]) = Translator.OrElse(self, that)

  def |||[X >: I, Y <: O](that: X => Option[Y]) = self || Translator.Simple[I, O](that)

  def ++(split: I => Option[I], join: O => Option[O]) =
    this || Translator.Junction(split, join)

  def ++[XI, XO](
      split: I => Option[XI], join: XO => Option[O]
      )(implicit oml: OptMapLift[I, O, XI, XO]) =
        this || Translator.MultiJunction(split, join)

  def as[X <: I, Y >: O] = Translator.Simple[X, Y](self)
}


object Translator{
  /**
   * Empty translator, always fails
   */
  case class Empty[I, O]() extends Translator[I, O]{
    def recTranslate(rec: => Translator[I, O]) = (_ : I) => None
  }

  /**
   * Simple translator given by a function. Ignores sub-translator
   */
  case class Simple[I,O](translate: I => Option[O]) extends Translator[I, O]{
    def recTranslate(rec: => Translator[I, O]) = translate
  }

  /**
   * Tries the first translator at top level, then the second. Is recursive.
   */
  case class OrElse[I, O](first: Translator[I, O], second: Translator[I, O]) extends Translator[I, O]{
    def recTranslate(rec: => Translator[I, O]) =
      (inp : I) => first.recTranslate(rec)(inp) orElse second.recTranslate(rec)(inp)
  }

  /**
   * Translator built from pattern matching and joining, but without any actual splitting.
   * In case of a match, applies sub-translator to result.
   */
  case class Junction[I, O](split: I => Option[I], join: O => Option[O]) extends Translator[I, O]{
    def recTranslate(rec: => Translator[I, O]) =
      (inp: I) => split(inp) flatMap((x: I) => rec(x).flatMap(join(_)))
  }

  /**
   * The main recursive translator:
   * 
   * @param split matches and optionally splits to a type XI derived from I, possibly starting with a sub-type of I.
   * @param join optionally combined from a type XO built from O, possibly starting with a sub-type of O.
   * 
   */
  case class MultiJunction[I, O, J <: I, L <: O, XI, XO](
      split: J => Option[XI], join: XO => Option[L]
      )(implicit oml: OptMapLift[I, O, XI, XO]) extends Translator[I, O]{

    def recTranslate(rec: => Translator[I, O]) = {
      case (inp: J) if (inp.isInstanceOf[J])=> split(inp) flatMap((x: XI) => oml.lift(rec.apply)(x).flatMap(join(_)))
      case _ => None
    }
  }

}

trait OptMapLift[I, O, XI, XO]{
  def lift(optMap: I => Option[O]): XI => Option[XO]
}


object OptMapLift{
  implicit def  idLift[I, O] = new OptMapLift[I, O, I, O]{
    def lift(optMap: I => Option[O]) = optMap
  }

  implicit def extendLift[I, O, J <: I] = new OptMapLift[I, O, J, O]{
    def lift(optMap: I => Option[O]) = (inp: J) => optMap(inp)
  }

  implicit def restrictLift[I, O, L <: O] = new OptMapLift[I, O, I, L]{
    def lift(optMap: I => Option[O]) =
      (inp : I) => optMap(inp) flatMap ((x: O) => Try(x.asInstanceOf[L]).toOption)
  }

  implicit def listLift[I, O, XI, XO](implicit lft: OptMapLift[I, O, XI, XO]) = new OptMapLift[I, O, List[XI], List[XO]]{
    def lift(optMap: I => Option[O]) =
    {
      val liftMap = lft.lift(optMap)
      def lstMap(inp: List[XI]) : Option[List[XO]] = inp match {
        case List() => Some(List())
        case x :: ys =>
          for (a <- liftMap(x); bs <- lstMap(ys)) yield a :: bs
      }
    lstMap _
    }
  }

  implicit def tuple2Lift[I, O, XI1, XO1, XI2, XO2](
      implicit lft1: OptMapLift[I, O, XI1, XO1], lft2 : OptMapLift[I, O, XI2, XO2]) =
        new OptMapLift[I, O, (XI1, XI2), (XO1, XO2)]{
    def lift(optMap: I => Option[O]) = {
      case (x, y) =>
        for (a <- lft1.lift(optMap)(x); b <- lft2.lift(optMap)(y)) yield (a, b)
      }
  }

  implicit def tuple3Lift[I, O, XI1, XO1, XI2, XO2, XI3, XO3](
      implicit lft1: OptMapLift[I, O, XI1, XO1], lft2 : OptMapLift[I, O, XI2, XO2], lft3: OptMapLift[I, O, XI3, XO3]) =
        new OptMapLift[I, O, (XI1, XI2, XI3), (XO1, XO2, XO3)]{
    def lift(optMap: I => Option[O]) = {
      case (x, y, z) =>
        for (a <- lft1.lift(optMap)(x); b <- lft2.lift(optMap)(y); c <- lft3.lift(optMap)(z)) yield (a, b, c)
      }
  }

  implicit def tuple4Lift[I, O, XI1, XO1, XI2, XO2, XI3, XO3, XI4, XO4](
      implicit lft1: OptMapLift[I, O, XI1, XO1], lft2 : OptMapLift[I, O, XI2, XO2],
      lft3: OptMapLift[I, O, XI3, XO3], lft4: OptMapLift[I, O, XI4, XO4]) =
        new OptMapLift[I, O, (XI1, XI2, XI3, XI4), (XO1, XO2, XO3, XO4)]{
    def lift(optMap: I => Option[O]) = {
      case (x, y, z, w) =>
        for (a <- lft1.lift(optMap)(x); b <- lft2.lift(optMap)(y);
        c <- lft3.lift(optMap)(z); d <- lft4.lift(optMap)(w)) yield (a, b, c, d)
      }
  }
}
