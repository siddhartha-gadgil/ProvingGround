package provingground.learning
import provingground._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

object Truncate{
  import PD._

  def apply[A](pd: PD[A], epsilon: Double) : FD[A] =
    if (epsilon > 1) FD.empty[A] else
    pd match {
    case fd: FD[u] =>
      val purged = fd.flatten.pmf.filter{case Weighted(x, p) => p > epsilon}
      val fdp = FD(purged)
      val tot = fdp.total
      if (tot > 0) fdp * (1 / tot) else FD.empty[A]
    case mx: Mixin[u] =>
      (Truncate(mx.first, epsilon / mx.p).safeNormalized * mx.p) ++ (Truncate(mx.second, epsilon / mx.q).safeNormalized * mx.q)
    case mx: MixinOpt[u] =>
      val scndPmf = Truncate(mx.second, epsilon / mx.q).pmf.collect{case Weighted(Some(a), p) => Weighted(a, p * mx.q)}
      (Truncate(mx.first, epsilon / mx.p).safeNormalized * mx.p) ++ (FD(scndPmf).safeNormalized * mx.q)
    case mx: Mixture[u] =>
      val fds = mx.weightedDists.map{case (d, p) => Truncate(d, epsilon / p) * p}
      fds.foldLeft(FD.empty[A])(_ ++ _)
    case Mapped(base, f) =>
      Truncate(base, epsilon).map(f)
    case FlatMapped(base, f) =>
      Truncate(
         Truncate(base, epsilon).flatMap((a) => Truncate(f(a), epsilon)),
         epsilon)
    case prod : Product[u, v] =>
      val pmf1 = Truncate(prod.first, epsilon).pmf
      val pmf2 = Truncate(prod.second, epsilon).pmf
      val pmf = for (Weighted(x, p) <- pmf1; Weighted(y, q) <- pmf2 if p * q > epsilon) yield Weighted((x, y), p * q)
      FD(pmf)
    case fibprod : FiberProduct[a, q, b] =>
      val pmf1map = Truncate(fibprod.base, epsilon).pmf.groupBy((we) => fibprod.quotient(we.elem))
      val fds = for ((q, wxs) <- pmf1map; Weighted(x, p) <- wxs) yield Truncate(fibprod.fibers(q), epsilon / p).map((y) => (x, y)) * p
      fds.foldLeft(FD.empty[A])(_ ++ _)
    case Conditioned(base, p) =>
      val td = Truncate(base, epsilon).filter(p)
      val tot = td.total
      if (tot > 0) td * (1 / tot) else td
    case Flattened(base) =>
      val pmf = Truncate(base, epsilon).pmf.collect{case Weighted(Some(a), p) => Weighted(a, p)}
      FD(pmf)
    case CondMapped(base, f) =>
      val pmf = Truncate(base, epsilon).map(f).pmf.collect{case Weighted(Some(a), p) => Weighted(a, p)}
      val td = FD(pmf)
      val tot = td.total
      if (tot > 0) td * (1 / tot) else td
    case Scaled(base, sc) =>
      Truncate(base, epsilon/ sc)
    case Sum(first, second) =>
      Truncate(first, epsilon) ++ Truncate(second, epsilon)
    case _ =>
      throw new IllegalArgumentException(s"probability distribution $pd cannot be truncated")
  }
}
