package provingground

import HoTT._

import upickle.default._

import UnicodeSyms.{Arrow => Arr, _}

/**
  * @author gadgil
  */
@deprecated("Use Expression language", "April 2016")
sealed trait TermExpr {

  def asTerm(implicit lp: LiteralParser): Term

  def asTyp(implicit lp: LiteralParser): Typ[Term] = asTerm match {
    case tp: Typ[_] => tp
    case _ =>
      throw (new IllegalArgumentException(s"expected type but found $asTerm"))
  }
}

@deprecated("Use Expression language", "April 2016")
object TermExpr {

  /**
    * TermExpr a = b
    */
  case class Equality(dom: TermExpr, lhs: TermExpr, rhs: TermExpr)
      extends TermExpr {
    def asTerm(implicit lp: LiteralParser) =
      IdentityTyp(dom.asTyp, lhs.asTerm, rhs.asTerm)

    override def toString = s"$lhs = $rhs"
  }

  /**
    * Symbolic variable with given type
    */
  case class TypedVar(name: String, typ: TermExpr) extends TermExpr {
    def asTerm(implicit lp: LiteralParser): Term = typ.asTyp.symbObj(name)

    override def toString = s"($name : $typ)"
  }

  case class TypedLiteral(lit: String, typ: TermExpr) extends TermExpr {
    def asTerm(implicit lp: LiteralParser) = lp.parse(typ.asTyp)(lit).get

    override def toString = s"($lit : $typ)"
  }

  case class VarTyp(name: String) extends TermExpr {
    def asTerm(implicit lp: LiteralParser): Term = SymbTyp(name)

    override def toString = s"$name"
  }

  /**
    * LambdaTermExpr maps to a lambda
    */
  case class LambdaExpr(x: TermExpr, y: TermExpr) extends TermExpr {
    def asTerm(implicit lp: LiteralParser) =
      lambda(x.asTerm)(y.asTerm)

    override def toString = s"$x $MapsTo $y"
  }

  case class PairExpr(first: TermExpr, second: TermExpr) extends TermExpr {
    def asTerm(implicit lp: LiteralParser) =
      mkPair(first.asTerm, second.asTerm)

    override def toString = s"($first, $second)"
  }

  /**
    * expression func(arg)
    */
  case class Apply(func: TermExpr, arg: TermExpr) extends TermExpr {
    def asTerm(implicit lp: LiteralParser): Term =
      fold(func.asTerm)(arg.asTerm)

    override def toString = s"($func $arg)"
  }

  /**
    * the first universe
    */
  case class UnivTerm(n: Int) extends TermExpr {
    def asTerm(implicit lp: LiteralParser) = Universe(n)

    override def toString = s"${UnivSym}"
  }

  /**
    * expression for A -> B
    */
  case class Arrow(lhs: TermExpr, rhs: TermExpr) extends TermExpr {
    def asTerm(implicit lp: LiteralParser): Typ[Term] = lhs.asTyp ->: rhs.asTyp

    override def toString = s"($lhs $Arr $rhs)"
  }

  case class SigmaExpr(fiber: TermExpr) extends TermExpr {
    def asTerm(implicit lp: LiteralParser) = fiber.asTerm match {
      case fib: Func[_, _] =>
        {
          val x = fib.dom.Var
          val fibre = lmbda(x)(fib(x).asInstanceOf[Typ[Term]])
          SigmaTyp(fibre)
        }
      case _ =>
        throw (new IllegalArgumentException(
            s"cannot construct Sigma Type with fibre ${fiber.asTerm}"))
    }

    override def toString = s"${Sigma}_($fiber)"
  }

  case class PiExpr(fiber: TermExpr) extends TermExpr {
    def asTerm(implicit lp: LiteralParser) = fiber.asTerm match {
      case fib: Func[_, _] =>
        {
          val x = fib.dom.Var
          val fibre = lmbda(x)(fib(x).asInstanceOf[Typ[Term]])
          PiTyp(fibre)
        }
      case _ =>
        throw (new IllegalArgumentException(
            s"cannot construct Pi Type with fibre ${fiber.asTerm}"))
    }
    override def toString = s"${Pi}_$fiber"
  }

  case class PlusTypExpr(first: TermExpr, second: TermExpr) extends TermExpr {
    def asTerm(implicit lp: LiteralParser) = PlusTyp(first.asTyp, second.asTyp)

    override def toString = s"$first + $second"
  }

  def pickle(expr: TermExpr) = write(expr)

  def simple(term: Term) = expr(LiteralParser.Empty)(term)

  def unpickle(str: String) = read[TermExpr](str)

  case class expr(lp: LiteralParser,
                  specialTerms: PartialFunction[Term, TermExpr] = Map.empty)
      extends TermRec[TermExpr] {
    def appln(func: TermExpr, arg: TermExpr): TermExpr = Apply(func, arg)

    def arrow(dom: TermExpr, codom: TermExpr): TermExpr = Arrow(dom, codom)

    def equality(dom: TermExpr, lhs: TermExpr, rhs: TermExpr): TermExpr =
      Equality(dom, lhs, rhs)

    def fromString(str: String)(implicit typ: Typ[Term]): TermExpr =
      TypedLiteral(str, apply(typ))

    def lambda(variable: TermExpr, typ: TermExpr, value: TermExpr): TermExpr =
      LambdaExpr(variable, value)

    def pair(first: TermExpr, second: TermExpr): TermExpr =
      PairExpr(first, second)

    def pi(fibre: TermExpr): TermExpr = PiExpr(fibre)

    def plus(first: TermExpr, second: TermExpr): TermExpr =
      PlusTypExpr(first, second)

    def sigma(fibre: TermExpr): TermExpr = SigmaExpr(fibre)

    def symbobj(term: provingground.HoTT.SymbObj[Term]): TermExpr =
      TypedVar(term.name.toString, apply(term.typ))

    def symbtyp(typ: provingground.HoTT.SymbTyp): TermExpr =
      VarTyp(typ.name.toString)

    def symbolic(name: AnySym, typ: provingground.HoTT.Typ[Term]): TermExpr =
      TypedVar(name.toString, apply(typ))

    def univ(n: Int) = UnivTerm(n)
  }
}

trait LiteralParser {
  def parse(typ: Typ[Term])(lit: String): Option[Term]

  def literal(term: Term): Option[String]

  def unapply(lit: String, typ: Typ[Term]) = parse(typ)(lit).get

  def apply(term: Term) = literal(term).getOrElse(term.toString)
}

object LiteralParser {
  case object Empty extends LiteralParser {
    def parse(typ: Typ[Term])(lit: String): Option[Term] = None

    def literal(term: Term): Option[String] = None
  }
}
