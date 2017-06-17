package misc

import spire.algebra._
import spire.math._
import spire.implicits._
import ammonite.ops._

object MianChowla {
  @annotation.tailrec
  def firstGap(set: Set[SafeLong], accum: SafeLong = 1): SafeLong = {
    if (set contains (accum)) firstGap(set, accum + 1) else accum
  }

  def xyz(as: Traversable[SafeLong]): Set[SafeLong] = {
    val set = as.toSet
    for (x <- set; y <- set; z <- set) yield (x + y - z)
  }

  def nextNum(as: Traversable[SafeLong], n: SafeLong) = firstGap(xyz(as), n)

  val seq: Stream[SafeLong] =
    Stream.from(1) map
      ((n: Int) =>
        if (n == 1) SafeLong(1)
        else nextNum(seq take (n - 1), n: SafeLong))

  def view(n: Int) = seq.take(n).toList

  def run(n: Int) = {
    val iter = seq.zipWithIndex.take(n).toIterator
    for ((x, y) <- iter)
      println((x, y + 1, log(x.toDouble) / log(y.toDouble + 1)))
  }

  def run(n: Int, file: String) = {
    val wd   = pwd / "data"
    val iter = seq.zipWithIndex.take(n).toIterator
    for ((x, y) <- iter)
      write.append(wd / file, s"$x, ${y + 1}, ${log(x.toDouble) / log(y.toDouble + 1)}\n")
  }
}
