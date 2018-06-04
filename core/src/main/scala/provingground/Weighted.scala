package provingground

import annotation.tailrec

import scala.language.implicitConversions

import upickle.default._

import upickle.default.{ReadWriter => RW, macroRW}

case class Weighted[T](elem: T, weight: Double) {
  def scale(s: Double) = Weighted(elem, weight * s)

  def map[S](f: T => S) = Weighted(f(elem), weight)
}

object Weighted {
  @tailrec final def pick[T](dist: Traversable[Weighted[T]], t: Double): T =
    if (t - dist.head.weight < 0) dist.head.elem
    else pick(dist.tail, t - dist.head.weight)

  implicit def rw[A: ReadWriter]: ReadWriter[Weighted[A]] = ???

  def sumWeigths[T](seq: Seq[Weighted[T]]) = seq.map(_.weight).sum

  private def gather[T](seq: Seq[Weighted[T]]) =
    Weighted(seq.head.elem, Weighted.sumWeigths(seq))

  private def gather[T](seq: Vector[Weighted[T]]) =
    Weighted(seq.head.elem, Weighted.sumWeigths(seq))

  def flatten[T](seq: Seq[Weighted[T]]) =
    (seq groupBy (_.elem) map (_._2) map (gather(_))).filter(_.weight != 0)

  def flatten[T](seq: Vector[Weighted[T]]) =
    (seq groupBy (_.elem) map (_._2) map (gather(_))).filter(_.weight != 0)

  def combine[T](seqs: Seq[Weighted[T]]*) = flatten(seqs.flatten)

  implicit def weights[T](tws: (T, Double)*) =
    for (tw <- tws) yield Weighted(tw._1, tw._2)
}

case class PickledWeighted(elem: String, weight: Double) {
  def map[S](f: String => S) = Weighted(f(elem), weight)
}

import upickle.default.{ReadWriter => RW, macroRW}

object PickledWeighted {
  def pickle[T](wtd: Weighted[T]) =
    PickledWeighted(wtd.elem.toString, wtd.weight)

  implicit def rw: RW[PickledWeighted] = macroRW
}
