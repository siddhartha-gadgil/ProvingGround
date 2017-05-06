package provingground

import scala.annotation._
import scala.util._

import scala.language.implicitConversions

import provingground.Collections._
import AdjDiffbleFunction._

object LearningSystem {

  trait LearningSystem[I, P, O] extends AdjDiffbleFunction[(I, P), O] {
    def apply(inp: I, param: P): O = this.func((inp, param))

    def update(feedback: O, inp: I, param: P, epsilon: Double)(
        implicit s: Shift[P]) =
      s(param, grad(inp, param)(feedback)._2, epsilon)

    def stack[Q, X](that: LearningSystem[O, Q, X]) = {

      def fwd(inp: I, pq: (P, Q)) = that(this(inp, pq._1), pq._2)

      def bck(inp: I, pq: (P, Q))(x: X) = {
        val p         = pq._1
        val q         = pq._2
        val midval: O = this(inp, p)
        val thatdiff  = that.grad((midval, q))(x)
        val odiff     = thatdiff._1
        val thisdiff  = grad((inp, p))(odiff)
        (thisdiff._1, (thisdiff._2, thatdiff._2))
      }

      LearningSystem(fwd, bck)
    }
  }

  object LearningSystem {
    def aggregate[L, I, P, O](
        edge: LearningSystem[I, P, O],
        base: Traversable[L])(implicit zero: O, ls: LinearStructure[O]) = {
      def fwd(inps: ArrayMap[L, I], params: ArrayMap[L, P]) = {
        val terms = for (k <- inps.coords.keys; in <- inps.get(k);
                         p <- params.get(k)) yield edge(in, p)
        terms.foldLeft(zero)(ls.sum(_, _))
      }

      def bck(inps: ArrayMap[L, I], params: ArrayMap[L, P])(o: O) = {
        val inpmap = (for (k <- inps.coords.keys; in <- inps.get(k);
                           p <- params.get(k))
          yield (k -> edge.grad(in, p)(o)._1)).toMap

        val parammap = (for (k <- inps.coords.keys; in <- inps.get(k);
                             p <- params.get(k))
          yield (k -> edge.grad(in, p)(o)._2)).toMap

        (ArrayMap(inpmap, inps.supp), ArrayMap(parammap, params.supp))
      }

      LearningSystem(fwd, bck)
    }

    def apply[I, P, O](fwd: (I, P) => O, bck: (I, P) => O => (I, P)) = {
      def forward(ip: (I, P)) = fwd(ip._1, ip._2)

      def back(ip: (I, P))(out: O): (I, P) = back(ip._1, ip._2)(out)

      asLearner(AdjDiffbleFunction[(I, P), O](forward)(back))
    }

    def edge[I](f: AdjDiffbleFunction[I, Double]) = {
      def fwd(inp: I, wt: Double) = wt * f.func(inp)

      def bck(inp: I, wt: Double)(o: Double) =
        (f.grad(inp)(wt * o), fwd(inp, wt))

      LearningSystem[I, Double, Double](fwd, bck)
    }

    def collect[I, P, O, E](comps: Map[E, LearningSystem[I, P, O]])(
        implicit zi: I,
        zp: P,
        zo: O,
        ls: LinearStructure[I]) = {
      val exits = comps.keys

      def fwd(inp: I, param: ArrayMap[E, P]) =
        ArrayMap((exits map ((k) => (k -> comps(k)(inp, param(k))))).toMap)

      def bck(inp: I, param: ArrayMap[E, P])(out: ArrayMap[E, O]) = {
        val parammap = (for (e <- exits)
          yield (e -> comps(e).grad(inp, param(e))(out(e))._2)).toMap

        val inpterms = for (e <- exits)
          yield
            comps(e)
              .grad(inp, param(e))(out(e))
              ._1

        ((zi /: inpterms)(ls.sum(_, _)), ArrayMap(parammap))
      }

      LearningSystem[I, ArrayMap[E, P], ArrayMap[E, O]](fwd, bck)
    }

    def ANN[D, C](f: AdjDiffbleFunction[Double, Double],
                  dom: Traversable[D],
                  codom: Traversable[C],
                  inc: (D, C) => Boolean) = {
      val ed = edge(f)

      def toexit(y: C) = aggregate(ed, dom filter (inc(_, y)))

      val exitMap = (for (y <- codom) yield (y -> toexit(y))).toMap

      collect(exitMap)
    }

    def tuner[P, O](evol: AdjDiffbleFunction[P, O]) = {
      def fwd: (Unit, P) => O = { (_, p) =>
        evol.func(p)
      }
      def back(u: Unit, param: P)(err: O) = ({}, evol.grad(param)(err))
      LearningSystem(fwd, back)
    }
  }

  implicit def asLearner[I, P, O](
      f: AdjDiffbleFunction[(I, P), O]): LearningSystem[I, P, O] =
    new LearningSystem[I, P, O] {
      lazy val func = (a: (I, P)) => f.func(a)

      lazy val adjDer = (a: (I, P)) => f.grad(a)
    }

  def learn[I, P, O](learner: LearningSystem[I, P, O],
                     feedback: (O, O) => O,
                     epsilon: Double)(implicit s: Shift[P]): (I, P, O) => P = {
    (inp, param, target) =>
      learner.update(feedback(target, learner(inp, param)),
                     inp,
                     param,
                     epsilon)
  }
}
