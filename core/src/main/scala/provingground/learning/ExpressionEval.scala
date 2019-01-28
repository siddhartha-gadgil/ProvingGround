package provingground.learning
import provingground.{FiniteDistribution => FD, _}
import shapeless.HList._
import shapeless._

import monix.eval._

import GeneratorVariables._, Expression._


import MonixFiniteDistributionEq._

import scala.util.Try

object ExpressionEval{
    val sd = implicitly[StateDistribution[TermState, FD]]

    def initVal(exp: Expression, tg: TermGenParams, ts: TermState) : Option[Double] = 
        exp match {
            case cf @ Coeff(_, _) => cf.get(tg.nodeCoeffSeq)
            case InitialVal(Elem(el, rv)) => 
                val base = sd.value(ts)(rv)(el)
                if (base >0) Some(base) else Some(tg.varWeight / (1 - tg.varWeight)) // for the case of variables in islands
            case IsleScale(_, _) => Some((1.0 - tg.varWeight))
            case _ => None
        }

    def initMap(atoms: Set[Expression], tg: TermGenParams, ts: TermState) : Map[Expression, Double] = 
        (for {
            exp <- atoms
            value <- initVal(exp, tg, ts)
        } yield exp -> value).toMap

    def recExp(init: Map[Expression, Double], exp: Expression) : Double = 
        init.getOrElse(exp, exp match
            {
                case Sum(a, b) => recExp(init, a) + recExp(init, b)
                case Log(a)             => math.log(recExp(init, a))
                case Product(x, y)        => recExp(init, x) * recExp(init, y)
                case Literal(x)           => x
                case Quotient(x, y)       => if (recExp(init, y) != 0)  recExp(init, x) / recExp(init, y) else 0
                case _ => 0
            }
        )

    def nextMap(init: Map[Expression, Double], equations: Set[Equation]) : Map[Expression, Double] =
            {
                init ++ equations.map(eq => eq.lhs -> recExp(init, eq.rhs))
            }.toMap
}