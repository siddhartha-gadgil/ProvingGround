package provingground.learning
import scala.util._
import provingground.Weighted

class BackPaths[A](val generators: Map[A, Set[Set[A]]]) {
  val rnd = new Random()

  val initialValues = generators.keys.toVector

  def randomPathsFromInit(init: A, length: Int): Set[Vector[A]] =
    if (length == 0) Set(Vector())
    else {
      val branches: Vector[Set[A]] =
        generators.getOrElse(init, Set.empty[Set[A]]).toVector
      if (branches.isEmpty) Set(Vector(init))
      else {
        val gens = branches(rnd.nextInt(branches.size))
        gens.flatMap { a =>
          randomPathsFromInit(a, length - 1).map { v =>
            init +: v
          }
        }
      }
    }

  def randomPaths(initNumber: Int, length: Int): Vector[Vector[A]] = {
    val inits: Vector[A] = (1 to initNumber).toVector
      .map(_ => initialValues(rnd.nextInt(initialValues.size)))
    inits.flatMap(a => randomPathsFromInit(a, length).toVector)
  }
}

case class WeightedBackPaths[A, C](
    gens: Map[A, Vector[(C, Vector[A])]],
    coeffWeights: C => Double
) extends BackPaths[A](
      gens.mapValues(v => v.map { case (c, v) => v.toSet }.toSet)
    ) {
  def randomNext(init: A): Option[A] =
    gens.get(init).map { v =>
      val total: Double = v.map(cv => coeffWeights(cv._1)).sum
      val wb            = v.map { case (c, x) => Weighted(x, coeffWeights(c) / total) }
      val branch        = Weighted.pick(wb, rnd.nextDouble())
      branch(rnd.nextInt(branch.size))
    }

  def randomPathFromInit(init: A, length: Int): Vector[A] =
    if (length < 1) Vector()
    else
      randomNext(init)
        .map { x =>
          init +: randomPathFromInit(x, length - 1)
        }
        .getOrElse(Vector(init))

  def unifRandomPaths(number: Int, length: Int) = {
    val inits: Vector[A] = (1 to number).toVector
      .map(_ => initialValues(rnd.nextInt(initialValues.size)))
    inits.map(randomPathFromInit(_, length))
  }
}
