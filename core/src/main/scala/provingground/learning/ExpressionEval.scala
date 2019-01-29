package provingground.learning
import provingground.{FiniteDistribution => FD, _}
import shapeless.HList._
import shapeless._

import monix.eval._

import GeneratorVariables._, Expression._

import annotation.tailrec 


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
                case Quotient(x, y)       => if (recExp(init, y) != 0)  recExp(init, x) / recExp(init, y) else recExp(init, x)
                case _ => 0
            }
        )

    def nextMap(init: Map[Expression, Double], equations: Set[Equation]) : Map[Expression, Double] =
            {
                init ++ equations.map(eq => eq.lhs -> recExp(init, eq.rhs)).filter(_._2 != 0)
            }.toMap

    @tailrec
    def stableSupportMap(init: Map[Expression, Double], equations: Set[Equation]) : Map[Expression, Double] = {
        val newMap = nextMap(init, equations)
        if (newMap.keySet == init.keySet) newMap else stableSupportMap(newMap, equations)
    }

    @tailrec
    def iterateMap(init: Map[Expression, Double], equations: Set[Equation], steps: Int) : Map[Expression, Double] = 
        if (steps < 1) init else iterateMap(nextMap(init, equations), equations, steps - 1)


    def mapRatio[A](m1: Map[A, Double], m2: Map[A, Double]) : Double = {
        require(m1.keySet == m2.keySet, "comparing maps with different supports")
        m1.map{case (k, v) => math.max(v / m2(k), (m2(k) / v))}.max
    }

    @tailrec
    def stableMap(init: Map[Expression, Double], equations: Set[Equation], maxRatio: Double = 1.01) : Map[Expression, Double] = {
        val newMap = nextMap(init, equations)
        if ((newMap.keySet == init.keySet) && mapRatio(newMap, init) < maxRatio) newMap else stableMap(newMap, equations, maxRatio)
    }
}

trait EvolvedEquations[State, Boat]{
    val initState : State 
    val finalState : State
    val equations: Set[Equation]
  
    def totalSquare(epsilon: Double): Expression =
    equations.map(_.squareError(epsilon)).reduce(_ + _)
  
    def mse(epsilon: Double): Expression = totalSquare(epsilon) / (equations.size)
    
  
  }
  