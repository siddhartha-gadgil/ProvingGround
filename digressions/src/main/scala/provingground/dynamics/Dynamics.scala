package provingground.dynamics

import provingground._
import Structures._
import annotation.tailrec
import scala.util._
import akka.actor._
import scala.concurrent._
import scala.concurrent.duration._
import provingground.HoTT._

object Goals {
  trait Goal[U] {
    def achieves(soln: U): Boolean
  }

  case class FindSolution[U, V](target: V, fn: U => V) extends Goal[U] {
    def achieves(soln: U) = fn(soln) == target
  }

  trait Reduction[U] {
    val target: Goal[U]

    val newGoals: Set[Goal[U]]

    def solution(solns: Goal[U] => Option[U]): Option[U]

    def apply(solns: Goal[U] => Option[U]) = solution(solns)

    def apply(solns: PartialFunction[Goal[U], U]) = solution(solns.lift)
  }

  case class SimpleReduction[U](target: Goal[U],
                                newGoal: Goal[U],
                                reduction: U => U)
      extends Reduction[U] {
    val newGoals = Set(newGoal)

    def solution(solns: Goal[U] => Option[U]) = solns(newGoal) map (reduction)

    def reduce: U => Option[U] = {
      case soln if newGoal.achieves(soln) => Some(reduction(soln))
      case _                              => None
    }

    def apply(soln: U) = reduce(soln)
  }
}

object Entropy {
  type Resolver[S] = S => Option[List[S]]

  case class Relation[S](src: S, target: S) extends Resolver[S] {
    def apply(s: S) = if (s == src) Some(List(target)) else None
  }

  object Resolver {
    def fromTuple[S](unapp: S => Option[Product]): Resolver[S] = {
      case (s) =>
        unapp(s) map (_.productIterator.toList.map(_.asInstanceOf[S]))
    }

    def apply[S](unapp: S => Option[Product]): Resolver[S] = fromTuple(unapp)
  }

  trait Rec[S] {

    /** The given entropies */
    val base: S => Option[Double]

    /** Resolving objects into others */
    val resolvers: Set[Resolver[S]]

    /** The cost of resolving */
    val resolverWeight: Resolver[S] => Double

    private def sum(a: Double, b: Double) = a + b

    private def entSum(l: List[S],
                       bound: Double,
                       withBase: S => Option[Double]): Double =
      l map ((s) => recEntropy(s, bound)) reduce (sum)

    def recEntropy(s: S,
                   bound: Double,
                   withBase: S => Option[Double] = base): Double = {
      if (bound < 0) 0
      else {
        base(s) match {
          case Some(ent) if ent < bound =>
            math.min(ent, recEntropy(s, ent, withBase))
          case _ =>
            val offspring = for (res <- resolvers; l <- res(s))
              yield
                (entSum(l, bound - resolverWeight(res), withBase) + resolverWeight(
                  res))
            (offspring + (bound + 1)).min
        }
      }
    }

    def entropyBounded(bound: Double)(s: S) = recEntropy(s, bound) < bound

    def addBase(newBase: S => Option[Double])(s: S) = base(s) orElse newBase(s)

    def addGens(gens: Set[S], weight: Double = 1)(s: S) = {
      val newBase: S => Option[Double] = {
        case s =>
          if (gens contains (s)) Some(weight) else None
      }
      addBase(newBase)(s)
    }

    @tailrec final def cumulEntropy(ss: List[S],
                                    bound: Double,
                                    accum: Double = 0): Double = ss match {
      case List()  => 0
      case List(s) => recEntropy(s, bound)
      case s :: l =>
        cumulEntropy(l, bound, accum + recEntropy(s, bound, addGens(l.toSet)))
    }

    def cmpndRecEntropy(s: S, bound: Double): Double = {
      if (bound < 0) 0
      else {
        base(s) match {
          case Some(ent) if ent < bound =>
            math.min(ent, cmpndRecEntropy(s, ent))
          case _ =>
            val offspring = for (res <- resolvers; l <- res(s))
              yield
                (cumulEntropy(l, bound - resolverWeight(res)) + resolverWeight(
                  res))
            (offspring + (bound + 1)).min
        }
      }
    }
  }

  object Rec {
    def unionBase[S](bs: List[S => Option[Double]])(s: S) =
      bs.map(_(s)).foldLeft(None: Option[Double])(_ orElse _)
  }

  case class SimpleRecData[S](gen: Set[S], resolvers: Set[Resolver[S]])
      extends Rec[S] {
    val base: S => Option[Double] = {
      case s =>
        if (gen contains (s)) Some(1) else None
    }

    val resolverWeight: Resolver[S] => Double = { case r => 1 }
  }
}

object Dynamics {

  trait Feedback[I, O] {
    def feedback(value: O, trueVal: O, arg: I, epsilon: Double): I
  }

  case class FeedbackFunction[I, O](forward: I => O,
                                    back: (O, O, I, Double) => I)
      extends Feedback[I, O]
      with (I => O) {
    def feedback(value: O, trueVal: O, arg: I, epsilon: Double) =
      back(value, trueVal, arg, epsilon)

    def apply(arg: I) = forward(arg)
  }

  trait SimpleGradientFlow[I, O] {
    def dyn(fn: FeedbackFunction[I, O]): I => I
  }

  case class GradFlowPoint[I, O](point: I, flow: GradFlow[I, O])

  trait GradFlow[I, O] {
    def dyndyn(fn: FeedbackFunction[I, O]): I => GradFlowPoint[I, O]
  }

  // e.g. Represent Integer as product of primes.
  trait Represent[A, B] {
    def construct(src: A): Option[B]
    def resolve(target: B): Option[A]
  }

  trait SimpleRep[A, B] extends Represent[A, B] {
    def build(src: A): B
    def construct(src: A) = Some(build(src))
  }

  case class Representation[S, T](src: S, func: S => T) {
    lazy val target = func(src)
  }

  trait DynSys[A] {
    def act: Set[A] => DynState[A]

    def mixin[B](isle: => DynIsle[A, B]): DynSys[A] =
      DynDynSys((s: Set[A]) => {
        val nextIsle = isle.next
        DynState(s union (nextIsle.state map (isle.mapBy)), mixin(nextIsle))
      })

    def mixinSet[B](isles: => Set[DynIsle[A, B]]): DynSys[A] =
      DynDynSys((s: Set[A]) => {
        val nextIsles = isles map (_.next)
        val export =
          for (isle <- nextIsles; b <- isle.state) yield (isle.mapBy(b))
        DynState(s union export, mixinSet(nextIsles))
      })

    def spawn[B](isle: => DynIsle[A, B]) =
      DynDynSys((s: Set[A]) => {
        DynState(act(s).state, act(s).dyn mixin isle)
      })

    def spawnSet[B](isles: Set[A] => Set[DynIsle[A, B]]) =
      DynDynSys((s: Set[A]) => {
        DynState(act(s).state, act(s).dyn mixinSet isles(s))
      })
  }

  object DynSys {
    def id[A] = StatDynSys((s: Set[A]) => s)

    def lift[A](f: A => A) = StatDynSys((s: Set[A]) => (s map f))

    def plift[A](pf: PartialFunction[A, A]) =
      StatDynSys((s: Set[A]) => (s collect pf))

    def pairs[A](pairing: PartialFunction[(A, A), A]) = StatDynSys(
      (s: Set[A]) => (for (x <- s; y <- s) yield (x, y)) collect pairing
    )

    def chPairs[A](pairing: PartialFunction[A, PartialFunction[A, A]]) =
      StatDynSys(
        (s: Set[A]) => {
          val fns = s collect pairing
          fns flatMap (fn => s collect fn)
        }
      )

    def triples[A](tripling: PartialFunction[(A, A, A), A]) = StatDynSys(
      (s: Set[A]) =>
        (for (x <- s; y <- s; z <- s) yield (x, y, z)) collect tripling
    )

    def apply[A](pf: PartialFunction[A, A]) =
      StatDynSys((s: Set[A]) => s collect pf)
  }

  case class DynState[A](state: Set[A], dyn: DynSys[A]) {
    def next = dyn.act(state)

    @tailrec final def recSteps(n: Int,
                                sofar: DynState[A] = this): DynState[A] = {
      if (n < 1) sofar else recSteps(n - 1, sofar.next)
    }

    def get(n: Int) = recSteps(n: Int).state
  }

  case class DynDynSys[A](act: Set[A] => DynState[A]) extends DynSys[A]

  case class StatDynSys[A](step: Set[A] => Set[A])
      extends DynSys[A]
      with Function1[Set[A], Set[A]] {

    def act = (s: Set[A]) => DynState(step(s), this)

    def apply(s: Set[A]) = step(s)

    def union(thatStep: Set[A] => Set[A]) =
      StatDynSys((s: Set[A]) => step(s) union thatStep(s))

    def ++(thatStep: Set[A] => Set[A]) = union(thatStep)

    def andThen(thatStep: Set[A] => Set[A]) =
      StatDynSys((s: Set[A]) => thatStep(step(s)))
  }

  case class DelaySys[A](dyn: DynSys[A]) extends DynSys[A] {
    def act = (s: Set[A]) => DynState(s, dyn)
  }

  case class DynIsle[A, B](isleState: DynState[B], mapBy: B => A) {
    def next  = DynIsle(isleState.next, mapBy)
    def state = isleState.state
  }

  object HottDyn {
    type Pairing = PartialFunction[(Term, Term), Term]

    type CHPairing = PartialFunction[Term, PartialFunction[Term, Term]]

//       def Applications[W<: Term, V<: Typ[U], U<: Term]: Pairing = {
//           case (f: Func[_, _, _], x: Term) if f.dom == x.typ => f(x).get
//       }

    def applyFn[W <: Term with Subs[W], U <: Term with Subs[U]](
        f: Func[W, U]): PartialFunction[Term, Term] = {
      case arg if arg.typ == f.dom => f(arg.asInstanceOf[W])
    }
    /*
       val applications: PartialFunction[Term, PartialFunction[Term, Term]] = {
         case f: Func[_,_] => applyFn(f)
       }
     */

    def logicalArrows[V <: Term]: Pairing = {
      case (dom: Typ[Term], codom: Typ[_]) => FuncTyp[Term, Term](dom, codom)
    }

    def lambdaIsles(dyn: => DynSys[Term])(state: Set[Term]) = {
      val newVarSym = nextChar(usedChars(state)).toString
      val gens: PartialFunction[Term, DynIsle[Term, Term]] = {
        case typ: Typ[Term] =>
          val obj = typ.symbObj(newVarSym.toString)
          DynIsle(DynState(state + obj, dyn), lambda(obj) _)
      }
      state collect gens
    }

    val inferenceDyn: DynSys[Term] =
      DynSys.id[Term] ++ DynSys.pairs(logicalArrows) spawnSet
        (lambdaIsles(inferenceDyn) _)

//	val InferenceDyn = Dyn.id[Term] ++ Dyn.pairs(LogicalArrows) andThen (expandGens _) mixin (lambdaGens _)
  }
}
