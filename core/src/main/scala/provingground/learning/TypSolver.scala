package provingground.learning
import provingground._, HoTT._


class TypSolver(val solve: Typ[Term] => Option[Term] = (_) => None)
  extends (Typ[Term] => Option[Term]) {
  def apply(typ: Typ[Term]): Option[Term] = solve(typ).map(_ !: typ)

  def ||(that: TypSolver): TypSolver = TypSolver.OrElse(this, that)

  val toJson: ujson.Value = ujson.Str(toString)

}

object TypSolver {
  def fromJson(js: ujson.Value, solvers: Vector[TypSolver]) : TypSolver = js match {
    case ujson.Str(value) => solvers.find(_.toString == value).getOrElse(throw new Exception(s"$js does not correspond to a type solver"))
    case ujson.Obj(value) => OrElse(fromJson(value("first"), solvers), fromJson(value("second"), solvers))
    case _ => throw new Exception(s"$js does not correspond to a type solver")
  }

  case class OrElse(fst: TypSolver, snd: TypSolver)
    extends TypSolver((x: Typ[Term]) => fst.solve(x) orElse (snd.solve(x))) {
    override val toJson: ujson.Value =
      ujson.Obj("first" -> fst.toJson, "second" -> snd.toJson)
  }

  case object IdSolver
    extends TypSolver({
      case id: IdentityTyp[u] if id.lhs == id.rhs => Some(id.lhs.refl)
      case _                                      => None
    })

  import scalahott._
  case object LeqNatSolver
    extends TypSolver({
      case NatRing.LEQ(a, b) => NatRing.findLEQ(a, b)
      case _                 => None
    })

  case object DivNatSolver
    extends TypSolver({
      case NatRing.DIV(a, b) => NatRing.findDivisibility(a, b)
      case _                 => None
    })

  case object LeqQSolver extends TypSolver({
    case QField.Pos(x) => QField.showPositive(x)
    case _ => None
  })

  implicit val coreSolver : TypSolver = IdSolver || LeqNatSolver || DivNatSolver || LeqQSolver
}
