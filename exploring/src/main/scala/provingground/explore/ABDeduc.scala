package provingground.explore

import provingground.{FiniteDistribution => FD}

import provingground._

import ammonite.ops._

import HoTT._

object ABDeduc {
  import HoTT.Type
  val A = "A" :: HoTT.Type

  val B = "B" :: Type

  val a = "a" :: A

  val b = "b" :: B

  val distAB = FD.unif[Term](A, B)

  val cnst = lmbda(a)(lmbda(b)(a))

  val f = "f" :: (A ->: B)

  val g = "g" :: (A ->: A)

  val idA = lmbda(a)(a)

  val varsAB = Vector(A, B) map (Weighted[Term](_, 0.3))

  val ded = Deducer(vars = varsAB)

  val X = "X" :: Type

  def swap(t: Term) = t.replace(A, X).replace(B, A).replace(X, B)

  def smooth(fd: FD[Term]) = ((fd * 0.5) ++ ((fd map (swap)) * 0.5)).flatten

  val dedFine = Deducer(cutoff = 0.001)

  val dir = pwd / "data" / "deduc-explore"

  import FreeExpr.writeDist

  def save(t: FD[Term]) = write.append(dir / "AB.fds", writeDist(t) + "\n")

  def save2(t: FD[Term]) = write.append(dir / "AB2.fds", writeDist(t) + "\n")

  def saveFine(t: FD[Term]) =
    write.append(dir / "ABfine.fds", writeDist(t) + "\n")

  val buf =
    new ded.BufferedRun(distAB, 10000000, 100000, (_) => false, save, smooth)

  val bufQuick =
    new ded.BufferedRun(distAB, 100000, 10000, (_) => false, (_) => (), smooth)

  val bufFine = new dedFine.BufferedRun(
      distAB, 10000000, 100000, (_) => false, saveFine, smooth)

  val twodays = 1000.toLong * 3600 * 24 * 2

  val buf2Days = new ded.BufferedRun(distAB,
                                     10000000,
                                     100000,
                                     (b) => b.getElapsedTime > twodays,
                                     save2,
                                     smooth)
}
