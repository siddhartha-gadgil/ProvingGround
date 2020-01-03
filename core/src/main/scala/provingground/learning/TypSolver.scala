package provingground.learning
import provingground._, HoTT._

trait TypSolver extends (Typ[Term] => Option[Term]) {
  val solve: Typ[Term] => Option[Term]

  def apply(typ: Typ[Term]): Option[Term] = solve(typ).map(_ !: typ)

  def ||(that: TypSolver): TypSolver = TypSolver.OrElse(this, that)

  val toJson: ujson.Value = ujson.Str(toString)

}

object TypSolver {
  def fromJson(js: ujson.Value, solvers: Vector[TypSolver]): TypSolver =
    js match {
      case ujson.Str(value) =>
        solvers
          .find(_.toString == value)
          .getOrElse(
            throw new Exception(s"$js does not correspond to a type solver")
          )
      case ujson.Obj(value) =>
        OrElse(
          fromJson(value("first"), solvers),
          fromJson(value("second"), solvers)
        )
      case _ => throw new Exception(s"$js does not correspond to a type solver")
    }

  case class OrElse(fst: TypSolver, snd: TypSolver) extends TypSolver {
    val solve = { (x: Typ[Term]) =>
      fst.solve(x) orElse (snd.solve(x))
    }
    override val toJson: ujson.Value =
      ujson.Obj("first" -> fst.toJson, "second" -> snd.toJson)
  }

  case object IdSolver extends TypSolver {
    val solve = {
      case id: IdentityTyp[u] if id.lhs == id.rhs => Some(id.lhs.refl)
      case _                                      => None
    }
  }

  import scalahott._, NatRing._
  case object LeqNatSolver extends TypSolver {
    val solve = {
      case NatRing.LEQ(a, b) => NatRing.findLEQ(a, b)
      case _                 => None
    }
  }

  case object DivNatSolver extends TypSolver {
    val solve = {
      case NatRing.DIV(a, b) => NatRing.findDivisibility(a, b)
      case _                 => None
    }
  }

  case object LeqQSolver extends TypSolver {
    val solve = {
      case QField.Pos(x) => QField.showPositive(x)
      case _             => None
    }
  }

  import provingground.library.NatDecEq
  case object NatNEQSolver extends TypSolver {
    val solve = {
      case FuncTyp(idt: IdentityTyp[u], cod) if idt.dom == NatTyp && cod == Zero  =>
        NatDecEq.showNatGT(idt.lhs.asInstanceOf[Nat], idt.rhs.asInstanceOf[Nat])
      case _ => None
    }
  }

  val coreSolver
      : TypSolver = IdSolver || LeqNatSolver || DivNatSolver || LeqQSolver || NatNEQSolver

  case class HashLookupSolver(terms: Set[Term]) extends TypSolver {
    val typs = terms.map(t => t.typ)

    val solve = typ =>
      if (typs.contains(typ)) Some(s"lookup$hashCode" :: typ) else None
  }

  case class LookupSolver(terms: Set[Term]) extends TypSolver {
    val m = terms.map(t => (t.typ: Typ[Term], t)).toMap

    val solve = (t) => m.get(t)
  }

}
