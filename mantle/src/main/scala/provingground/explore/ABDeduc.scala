package provingground.explore

import provingground.{FiniteDistribution => FD}

import provingground._

import ammonite.ops._

import HoTT._

object ABDeduc{
  import HoTT.Type
  val A = "A" :: HoTT.Type

  val B = "B" :: Type

  val a = "a" :: A

  val b = "b" :: B

  val distAB = FD.unif[Term](A, B)

  val cnst = lmbda(a)(lmbda(b)(a))

  val f = "f" :: (A ->: A)

  val g = "g" :: (A ->: A)

  val idA = lmbda(a)(a)

  val ded = Deducer()

  val X = "X" :: Type

  def swap(t: Term) = t.replace(A, X).replace(B, A).replace(X, B)

  def smooth (fd: FD[Term]) = (fd * 0.5) ++ ((fd map (swap)) * 0.5)

  val dedFine = Deducer(cutoff = 0.001)

  val dir = cwd / "data" / "deduc-explore"

  import FreeExprLang.writeDist

  def save(t: FD[Term]) = write.append(dir /"AB.fds", writeDist(t))

  def saveFine(t: FD[Term]) = write.append(dir /"ABfine.fds", writeDist(t))

  val buf = new ded.BufferedRun(distAB, 10000000, 100000, (_) => false, save, smooth)

  val bufQuick = new ded.BufferedRun(distAB, 100000, 10000, (_) => false, (_) => (), smooth)

  val bufFine = new dedFine.BufferedRun(distAB, 10000000, 100000, (_) => false, saveFine, smooth)
}
