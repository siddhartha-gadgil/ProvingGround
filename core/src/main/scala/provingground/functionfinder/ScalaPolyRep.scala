package provingground.functionfinder

import scala.language.implicitConversions

import provingground._, HoTT._

import scala.util._

trait ScalaPolyRep[U <: Term with Subs[U], W] {
  def apply(typ: Typ[Term])(elem: W): Option[U]

  def unapply(term: U): Option[W]

  import ScalaPolyRep._

  def ||(that: ScalaPolyRep[U, W]) = OrScalaPolyRep(this, that)

  def subs(x: Term, y: Term): ScalaPolyRep[U, W]
}

object ScalaPolyRep {

  case class ScalaRepWrap[U <: Term with Subs[U], W](rep: ScalaRep[U, W])
      extends ScalaPolyRep[U, W] {
    def apply(typ: Typ[Term])(elem: W) =
      if (typ == rep.typ) Some(rep(elem)) else None

    def unapply(term: U) = rep.unapply(term)

    def subs(x: Term, y: Term): ScalaPolyRep[U, W] =
      ScalaRepWrap(rep.subs(x, y))
  }

  case class OrScalaPolyRep[U <: Term with Subs[U], W](
      first: ScalaPolyRep[U, W],
      second: ScalaPolyRep[U, W])
      extends ScalaPolyRep[U, W] {
    def apply(typ: Typ[Term])(elem: W) =
      first(typ)(elem) orElse (second(typ)(elem))

    def unapply(term: U) = first.unapply(term) orElse (second.unapply(term))

    def subs(x: Term, y: Term): ScalaPolyRep[U, W] =
      OrScalaPolyRep(first.subs(x, y), second.subs(x, y))
  }

  case class DepFuncPolyRep[U <: Term with Subs[U],
                            W,
                            X <: Term with Subs[X],
                            Y](domrep: ScalaPolyRep[U, W],
                               codrep: ScalaPolyRep[X, Y])
      extends ScalaPolyRep[FuncLike[U, X], W => Y] {
    def apply(typ: Typ[Term])(elem: W => Y) = typ match {
      case typ @ FuncTyp(dom: Typ[U], codom: Typ[X]) =>
        Try(
          ExtendedFunction(elem,
                           domrep,
                           codrep,
                           typ.asInstanceOf[FuncTyp[U, X]])).toOption
      case typ @ PiDefn(x: Term, y: Typ[v]) =>
        Try(
          ExtendedDepFunction(
            elem,
            domrep,
            codrep,
            (x :-> y).asInstanceOf[Func[U, Typ[X]]])).toOption

      case typ @ PiTyp(fibers) =>
        Try(
          ExtendedDepFunction(elem,
                              domrep,
                              codrep,
                              fibers.asInstanceOf[Func[U, Typ[X]]])).toOption
      case _ => None
    }

    def unapply(term: FuncLike[U, X]) = term match {
      case ExtendedFunction(dfn: (W => Y), d, c, _)
          if d == domrep && c == codrep =>
        Some(dfn)
      case ExtendedDepFunction(dfn: Function1[W, Y], d, c, _)
          if d == domrep && c == codrep =>
        Some(dfn)
      case _ => None
    }

    def subs(x: Term, y: Term) =
      DepFuncPolyRep(domrep.subs(x, y), codrep.subs(x, y))
  }

  case class FuncPolyRep[U <: Term with Subs[U], W, X <: Term with Subs[X], Y](
      domrep: ScalaPolyRep[U, W],
      codrep: ScalaPolyRep[X, Y])
      extends ScalaPolyRep[Func[U, X], W => Y] {
    def apply(typ: Typ[Term])(elem: W => Y) = typ match {
      case typ @ FuncTyp(dom: Typ[U], codom: Typ[X]) =>
        Try(
          ExtendedFunction(elem,
                           domrep,
                           codrep,
                           typ.asInstanceOf[FuncTyp[U, X]])).toOption
      case _ => None
    }

    def unapply(term: Func[U, X]) = term match {
      case ExtendedFunction(dfn: (W => Y), d, c, _)
          if d == domrep && c == codrep =>
        Some(dfn)
      //    case ExtendedDepFunction(dfn: Function1[W, Y], d, c, _) if d == domrep && c ==codrep => Some(dfn)
      case _ => None
    }

    def subs(x: Term, y: Term) =
      FuncPolyRep(domrep.subs(x, y), codrep.subs(x, y))
  }

  case class PairPolyRep[U <: Term with Subs[U], W, X <: Term with Subs[X], Y](
      firstrep: ScalaPolyRep[U, W],
      secondrep: ScalaPolyRep[X, Y])
      extends ScalaPolyRep[AbsPair[U, X], (W, Y)] {
    def apply(typ: Typ[Term])(elem: (W, Y)) = typ match {
      case typ @ ProdTyp(first: Typ[U], second: Typ[X]) =>
        for (a <- firstrep(first)(elem._1); b <- secondrep(second)(elem._2))
          yield PairTerm(a, b)
      case typ @ SigmaTyp(fibers) =>
        for (a <- firstrep(fibers.dom.asInstanceOf[Typ[Term]])(elem._1);
             b <- secondrep(fibers(a).asInstanceOf[Typ[Term]])(elem._2))
          yield DepPair(a, b, fibers.asInstanceOf[Func[U, Typ[X]]])
      case _ => None
    }

    def unapply(term: AbsPair[U, X]) = term match {
      case PairTerm(first, second) =>
        for (a <- firstrep.unapply(first); b <- secondrep.unapply(second))
          yield (a, b)
      //   case ExtendedDepFunction(dfn: Function1[W, Y], d, c, _) if d == domrep && c ==codrep => Some(dfn)
      case _ => None
    }

    def subs(x: Term, y: Term) =
      PairPolyRep(firstrep.subs(x, y), secondrep.subs(x, y))
  }

  implicit class ScalaPolyTerm[U <: Term with Subs[U], W](elem: W)(
      implicit rep: ScalaPolyRep[U, W]) {
    def hott(typ: Typ[Term]) = rep(typ)(elem)

    def getTerm(typ: Typ[Term]) = hott(typ).get
  }

  implicit class PolyTermScala[U <: Term with Subs[U]](term: U) {
    type Rep[W] = ScalaPolyRep[U, W]
    def scala[W: Rep] = implicitly[ScalaPolyRep[U, W]].unapply(term)
  }

  implicit def depFuncPolyRep[U <: Term with Subs[U],
                              W,
                              X <: Term with Subs[X],
                              Y](
      implicit domrep: ScalaPolyRep[U, W],
      codrep: ScalaPolyRep[X, Y]): ScalaPolyRep[FuncLike[U, X], W => Y] =
    DepFuncPolyRep(domrep, codrep)

  implicit def poly[U <: Term with Subs[U], W](
      implicit rep: ScalaRep[U, W]): ScalaPolyRep[U, W] = ScalaRepWrap(rep)

  def funcPolyRep[U <: Term with Subs[U], W, X <: Term with Subs[X], Y](
      implicit domrep: ScalaPolyRep[U, W],
      codrep: ScalaPolyRep[X, Y]): ScalaPolyRep[Func[U, X], W => Y] =
    FuncPolyRep(domrep, codrep)

  /**
    * Formal extension of a function given by a definition and representations for
    * domain and codomain.
    */
  case class ExtendedFunction[U <: Term with Subs[U],
                              V,
                              X <: Term with Subs[X],
                              Y](dfn: V => Y,
                                 domrep: ScalaPolyRep[U, V],
                                 codomrep: ScalaPolyRep[X, Y],
                                 typ: FuncTyp[U, X])
      extends Func[U, X] {

    lazy val dom = typ.dom

    lazy val codom = typ.codom

    def newobj = typ.obj

    def act(u: U) = u match {
      case domrep(v) =>
        codomrep(codom)(dfn(v)).getOrElse(codom.symbObj(ApplnSym(this, u)))
      case _ => codom.symbObj(ApplnSym(this, u))
    }

    def subs(x: provingground.HoTT.Term, y: provingground.HoTT.Term) =
      (x, y) match {
        case (u, v: Func[U, X]) if u == this => v
        case _ =>
          ExtendedFunction((v: V) => dfn(v),
                           domrep.subs(x, y),
                           codomrep.subs(x, y),
                           typ.subs(x, y))
      }
  }

  case class ExtendedDepFunction[U <: Term with Subs[U],
                                 V,
                                 X <: Term with Subs[X],
                                 Y](dfn: V => Y,
                                    domrep: ScalaPolyRep[U, V],
                                    codomrep: ScalaPolyRep[X, Y],
                                    fibers: Func[U, Typ[X]])
      extends FuncLike[U, X] {

    lazy val dom = fibers.dom

    lazy val depcodom = (a: U) => fibers(a)

    lazy val typ = PiDefn(fibers)

    def newobj = PiDefn(fibers).obj

    def act(u: U) = u match {
      case domrep(v) =>
        codomrep(fibers(u))(dfn(v))
          .getOrElse(depcodom(u).symbObj(ApplnSym(this, u)))
      case _ => fibers(u).symbObj(ApplnSym(this, u))
    }

    // val domobjtpe: reflect.runtime.universe.Type = typeOf[U]

    // val codomobjtpe: reflect.runtime.universe.Type = typeOf[X]

    def subs(x: provingground.HoTT.Term, y: provingground.HoTT.Term) =
      (x, y) match {
        case (u, v: Func[U, X]) if u == this => v
        case _ =>
          ExtendedDepFunction((v: V) => dfn(v),
                              domrep.subs(x, y),
                              codomrep.subs(x, y),
                              fibers.subs(x, y))
      }
  }
}
