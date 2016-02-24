package provingground

trait Translator[I, O] {self =>
  def recTranslate(rec: => Translator[I, O]): I => Option[O]

  def apply(inp: I) = recTranslate(self)(inp)

  def ||(that: Translator[I, O]) = Translator.Or(this, that)

  def ++(split: I => Option[I], join: O => Option[O]) =
    this || Translator.Junction(split, join)

  def ++[XI, XO](
      split: I => Option[XI], join: XO => Option[O]
      )(implicit oml: OptMapLift[I, O, XI, XO]) =
        this || Translator.MultiJunction(split, join)
}


object Translator{
  case class Empty[I, O]() extends Translator[I, O]{
    def recTranslate(rec: => Translator[I, O]) = (_ : I) => None
  }

  case class Simple[I,O](translate: I => Option[O]) extends Translator[I, O]{
    def recTranslate(rec: => Translator[I, O]) = translate
  }

  case class Or[I, O](first: Translator[I, O], second: Translator[I, O]) extends Translator[I, O]{
    def recTranslate(rec: => Translator[I, O]) =
      (inp : I) => first.recTranslate(rec)(inp) orElse second.recTranslate(rec)(inp)
  }

  case class Junction[I, O](split: I => Option[I], join: O => Option[O]) extends Translator[I, O]{
    def recTranslate(rec: => Translator[I, O]) =
      (inp: I) => split(inp) flatMap((x: I) => rec(x).flatMap(join(_)))
  }

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
