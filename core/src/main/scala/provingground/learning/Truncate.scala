package provingground.learning
import provingground._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import monix.eval._

import scala.concurrent._, duration._

object Truncate {
  import PD._

  def apply[A](pd: PD[A], epsilon: Double): FD[A] =
    if (epsilon > 1) FD.empty[A]
    else
      pd match {
        case fd: FD[u] =>
          val purged = fd.flatten.pmf.filter {
            case Weighted(x, p) => p > epsilon
          }
          val fdp = FD(purged)
          val tot = fdp.total
          if (tot > 0) fdp * (1 / tot) else FD.empty[A]
        case mx: Mixin[u] => {
          (Truncate(mx.first, epsilon / mx.p).safeNormalized * mx.p) ++ (Truncate(
            mx.second,
            epsilon / mx.q).safeNormalized * mx.q)
        }.flatten
        case mx: MixinOpt[u] =>
          val scndPmf = Truncate(mx.second, epsilon / mx.q).pmf.collect {
            case Weighted(Some(a), p) => Weighted(a, p * mx.q)
          }
          ((Truncate(mx.first, epsilon / mx.p).safeNormalized * mx.p) ++ (FD(
            scndPmf).safeNormalized * mx.q)).flatten
        case mx: Mixture[u] =>
          val fds = mx.weightedDists.map {
            case (d, p) => Truncate(d, epsilon / p) * p
          }
          fds.foldLeft(FD.empty[A])(_ ++ _).flatten
        case Mapped(base, f) =>
          Truncate(base, epsilon).map(f)
        case FlatMapped(base, f) =>
          Truncate(
            Truncate(base, epsilon).flatMap((a) => Truncate(f(a), epsilon)),
            epsilon)
        case prod: Product[u, v] =>
          val pmf1 = Truncate(prod.first, epsilon).pmf
          val pmf2 = Truncate(prod.second, epsilon).pmf
          val pmf = for (Weighted(x, p) <- pmf1; Weighted(y, q) <- pmf2
                         if p * q > epsilon) yield Weighted((x, y), p * q)
          FD(pmf)
        case fibprod: FiberProduct[a, q, b] =>
          val pmf1map = Truncate(fibprod.base, epsilon).pmf.groupBy((we) =>
            fibprod.quotient(we.elem))
          val fds = for ((q, wxs) <- pmf1map; Weighted(x, p) <- wxs)
            yield
              Truncate(fibprod.fibers(q), epsilon / p).map((y) => (x, y)) * p
          fds.foldLeft(FD.empty[A])(_ ++ _).flatten
        case Conditioned(base, p) =>
          val td  = Truncate(base, epsilon).filter(p)
          val tot = td.total
          if (tot > 0) td * (1 / tot) else td
        case Flattened(base) =>
          val pmf = Truncate(base, epsilon).pmf.collect {
            case Weighted(Some(a), p) => Weighted(a, p)
          }
          FD(pmf)
        case CondMapped(base, f) =>
          val pmf = Truncate(base, epsilon).map(f).pmf.collect {
            case Weighted(Some(a), p) => Weighted(a, p)
          }
          val td  = FD(pmf)
          val tot = td.total
          if (tot > 0) td * (1 / tot) else td
        case Scaled(base, sc) =>
          Truncate(base, epsilon / sc)
        case Sum(first, second) => {
          Truncate(first, epsilon) ++ Truncate(second, epsilon)
        }.flatten
        case _ =>
          throw new IllegalArgumentException(
            s"probability distribution $pd cannot be truncated")
      }

  def task[A](pd: PD[A],
              epsilon: Double,
              maxtime: Duration,
              decay: Double = 0.8): Task[FD[A]] =
    Task.eval(epsilon > 1).flatMap[FD[A]] {
      case true => Task.pure(FD.empty[A])
      case false => {
        val tsk: Task[FD[A]] = (pd match {
          case fd: FD[u] =>
            Task.eval {
              val purged = fd.flatten.pmf.filter {
                case Weighted(x, p) => p > epsilon
              }
              val fdp = FD(purged)
              val tot = fdp.total
              if (tot > 0) fdp * (1 / tot) else FD.empty[A]
            }
          case mx: Mixin[u] =>
            Task
              .parZip2(task(mx.first, epsilon / mx.p, maxtime),
                    task(mx.second, epsilon / mx.q, maxtime))
              .map {
                case (first, second) => {
                  (first.safeNormalized * mx.p) ++ (second.safeNormalized * mx.q)
                }.flatten
              }
          case mx: MixinOpt[u] =>
            Task
              .parZip2(task(mx.first, epsilon / mx.p, maxtime),
                    task(mx.second, epsilon / mx.q, maxtime))
              .map {
                case (first, second) =>
                  val scndPmf = second.pmf.collect {
                    case Weighted(Some(a), p) => Weighted(a, p * mx.q)
                  }
                  ((first.safeNormalized * mx.p) ++ (FD(scndPmf).safeNormalized * mx.q)).flatten
              }
          case mx: Mixture[u] =>
            val fds = mx.weightedDists.map {
              case (d, p) =>
                task(d, epsilon / p, maxtime).map(_ * p)
            }
            Task
              .gatherUnordered(fds)
              .map(_.foldLeft(FD.empty[A])(_ ++ _).flatten)
          case Mapped(base, f) =>
            task(base, epsilon, maxtime).map(_.map(f))
          case FlatMapped(base, f) =>
            task(base, epsilon, maxtime).flatMap { (baseFD) =>
              {
                val fibs =
                  baseFD.pmf.map {
                    case Weighted(a, p) =>
                      task(f(a), epsilon / p, maxtime).map((fd) => fd * p)
                  }
                val fibTask = Task.gatherUnordered(fibs)
                fibTask.map(_.foldLeft(FD.empty[A])(_ ++ _).flatten)
              }
            }
          case prod: Product[u, v] =>
            Task.parZip2(task(prod.first, epsilon, maxtime),
                      task(prod.second, epsilon, maxtime)) map {
              case (first, second) =>
                val pmf1 = first.pmf
                val pmf2 = second.pmf
                val pmf = for (Weighted(x, p) <- pmf1; Weighted(y, q) <- pmf2
                               if p * q > epsilon)
                  yield Weighted((x, y), p * q)
                FD[A](pmf)
            }
          case fibprod: FiberProduct[a, q, b] =>
            for {
              baseFD <- task(fibprod.base, epsilon, maxtime)
              pmf1map = baseFD.pmf.groupBy((we) => fibprod.quotient(we.elem))
              fdtasks = for ((q, wxs) <- pmf1map; Weighted(x, p) <- wxs)
                yield
                  task(fibprod.fibers(q), epsilon / p, maxtime)
                    .map(_.map((y) => (x, y)) * p)
              fds <- Task.gatherUnordered(fdtasks)
            } yield fds.foldLeft(FD.empty[A])(_ ++ _).flatten
          case Conditioned(base, p) =>
            for {
              tdr <- task(base, epsilon, maxtime)
              td  = tdr.filter(p)
              tot = td.total
            } yield if (tot > 0) td * (1 / tot) else td
          case Flattened(base) =>
            for {
              fd <- task(base, epsilon, maxtime)
              pmf = fd.pmf.collect {
                case Weighted(Some(a), p) => Weighted(a, p)
              }
            } yield FD(pmf)
          case CondMapped(base, f) =>
            for {
              fd <- task(base, epsilon, maxtime)
              pmf = fd.map(f).pmf.collect {
                case Weighted(Some(a), p) => Weighted(a, p)
              }
              td  = FD(pmf)
              tot = td.total
            } yield if (tot > 0) td * (1 / tot) else td
          case Scaled(base, sc) =>
            task(base, epsilon / sc, maxtime)
          case Sum(first, second) =>
            Task.parZip2(task(first, epsilon, maxtime),
                      task(second, epsilon, maxtime)) map {
              case (fst, scnd) => (fst ++ scnd).flatten
            }
          case _ =>
            throw new IllegalArgumentException(
              s"probability distribution $pd cannot be truncated")
        })
        tsk.timeoutTo[FD[A]](maxtime.asInstanceOf[FiniteDuration],
                             Task.pure(FD.empty[A]))
      }
    }
}
