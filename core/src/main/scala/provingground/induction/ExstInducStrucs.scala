package provingground.induction

import provingground._, HoTT._
import shapeless._
import scala.language.existentials

import scala.util.Try
/**
  * term level inductive structures for runtime contexts
  */
trait ExstInducStrucs {
  def subs(x: Term, y: Term): ExstInducStrucs

  def recOpt[C <: Term with Subs[C]](dom: Term, cod: Typ[C]): Option[Term]

  def inducOpt(dom: Term, cod: Term): Option[Term]

  val constants: Vector[Term]

  def ||(that: ExstInducStrucs) = ExstInducStrucs.OrElse(this, that)
}

case class ExstInducDefn(typFamily: Term,
                         intros: Vector[Term],
                         ind: ExstInducStrucs) {
  def subs(x: Term, y: Term) =
    ExstInducDefn(typFamily.replace(x, y),
                  intros.map(_.replace(x, y)),
                  ind.subs(x, y))

  def introsTypGroups: Map[Typ[Term], Int] =
    intros.map((x) => x.typ).groupBy(identity).mapValues(_.size)

  def sameAs(that: ExstInducDefn) =
    (typFamily.typ == that.typFamily.typ) &&
      introsTypGroups == that.introsTypGroups.map {
        case (k, v) => k.replace(that.typFamily, typFamily)
      }
}

object ExstInducStrucs {
  import translation.TermPatterns.fm

  case class OrElse(first: ExstInducStrucs, second: ExstInducStrucs)
      extends ExstInducStrucs {
    def subs(x: Term, y: Term) = OrElse(first.subs(x, y), second.subs(x, y))

    def recOpt[C <: Term with Subs[C]](dom: Term, cod: Typ[C]): Option[Term] =
      first.recOpt(dom, cod).orElse(second.recOpt(dom, cod))

    def inducOpt(dom: Term, cod: Term): Option[Term] =
      first.inducOpt(dom, cod).orElse(second.inducOpt(cod, cod))

    val constants = first.constants ++ second.constants

  }

  case class LambdaInduc(variable: Term, struct: ExstInducStrucs)
      extends ExstInducStrucs {
    def subs(x: Term, y: Term) =
      LambdaInduc(variable.replace(x, y), struct.subs(x, y))

    def recOpt[C <: Term with Subs[C]](dom: Term, cod: Typ[C]): Option[Term] =
      dom match {
        case FormalAppln(fmly, arg) if arg.typ == variable.typ =>
          struct.subs(variable, arg).recOpt(dom, cod)
        case _ => None
      }

    def inducOpt(dom: Term, cod: Term): Option[Term] =
      dom match {
        case FormalAppln(fmly, arg) if arg.typ == variable.typ =>
          struct.subs(variable, arg).inducOpt(dom, cod)
        case _ => None
      }

    val constants =
      struct.constants
        .collect { case FormalAppln(f, a) if a == variable => f }
  }

  case class ConsSeqExst[SS <: HList, H <: Term with Subs[H], Intros <: HList](
      cs: ConstructorSeqTL[SS, H, Intros],
      intros: Vector[Term]
  ) extends ExstInducStrucs {
    def subs(x: Term, y: Term) =
      ConsSeqExst(cs.subs(x, y), intros.map(_.replace(x, y)))

    def recOpt[C <: Term with Subs[C]](dom: Term, cod: Typ[C]) =
      if (dom == cs.typ) Some(cs.recE(cod)) else None

    def inducOpt(dom: Term, cod: Term): Option[Term] =
      if (dom == cs.typ) Some(cs.inducE(fm[H](cs.typ, cod))) else None

    val constants: Vector[Term] = cs.typ +: intros
  }

  def get(typ: Term, intros: Vector[Term]): ExstInducStrucs =
    ConsSeqExst(
      ConstructorSeqTL.getExst(toTyp(typ), intros).value,
      intros
    )

  case class IndConsSeqExst[SS <: HList,
                            H <: Term with Subs[H],
                            F <: Term with Subs[F],
                            Index <: HList: TermList,
                            Intros <: HList](
      cs: IndexedConstructorSeqDom[SS, H, F, Index, Intros],
      intros: Vector[Term])
      extends ExstInducStrucs {
    val fmly: Term = cs.W

    def subs(x: Term, y: Term): ExstInducStrucs =
      IndConsSeqExst(cs.subs(x, y), intros.map(_.replace(x, y)))

    def recOpt[C <: Term with Subs[C]](dom: Term, cod: Typ[C]) =
      if (dom == cs.W) Some(cs.recE(cod)) else None

    def inducOpt(dom: Term, cod: Term): Option[Term] =
      if (dom == cs.W) Some(cs.inducE(cod)) else None

    val constants: Vector[Term] = intros
  }

  def getIndexed(typF: Term, intros: Vector[Term]): ExstInducStrucs = {
    val fmly = TypFamilyExst.getFamily(typF)
    val indTyp =
      fmly.IndexedConstructorSeqExst.getIndexedConstructorSeq(intros).value
    IndConsSeqExst(indTyp, intros)(fmly.subst)
  }

  case object Base extends ExstInducStrucs {
    def subs(x: Term, y: Term): Base.type = this

    val constants = Vector(Zero, Unit, Star)

    def recOpt[C <: Term with Subs[C]](dom: Term, cod: Typ[C]): Option[Term] =
      dom match {
        case pt: ProdTyp[u, v] =>
          Some(pt.rec(cod))
        case Zero =>
          Some(Zero.rec(cod))
        case Unit =>
          Some(Unit.rec(cod))
        case pt: PlusTyp[u, v] =>
          Some(pt.rec(cod))
        case pt: SigmaTyp[u, v] =>
          Some(pt.rec(cod))
        case idt: IdentityTyp[u] =>
          Some(IdentityTyp.rec(idt.dom, cod))
        case _ =>
          None
      }

    def inducOpt(dom: Term, cod: Term): Option[Term] =
      dom match {
        case pt: ProdTyp[u, v] =>
          val x    = pt.first.Var
          val y    = pt.second.Var
          for{
            tp  <- Try(fold(fold(cod)(x))(y).asInstanceOf[Typ[Term]]).toOption
            fmly = x :-> (y :-> tp)
          } yield pt.induc(fmly)
        case Zero =>
          Some(Zero.induc(fm(Zero, cod)))
        case Unit =>
          Some(Unit.induc(fm(Unit, cod)))
        case pt: PlusTyp[u, v] =>
          Some(pt.induc(fm(pt, cod)))
        case pt: SigmaTyp[u, v] =>
          val x    = pt.fibers.dom.Var
          val y    = pt.fibers(x).Var
          for{
            tp  <- Try(fold(fold(cod)(x))(y).asInstanceOf[Typ[Term]]).toOption
            fmly = x :-> (y :-> tp)
          } yield pt.induc(fmly)
        case idt: IdentityTyp[u] =>
          Some(
            IdentityTyp.induc(
              idt.dom,
              cod
                .asInstanceOf[
                  FuncLike[u, FuncLike[u, FuncLike[Equality[u], Typ[Term]]]]])
          )
        case _ =>
          None
      }

  }
}
