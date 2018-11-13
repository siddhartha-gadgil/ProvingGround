package provingground.learning

import org.platanios.tensorflow.api
import provingground._
import org.platanios.tensorflow.api._

case class TFDist[A](pmfMap: Map[A, Output[Double]]) {
  val pmfVec: Vector[(A, Output[Double])] = pmfMap.toVector

  def ++(that: TFDist[A]): TFDist[A] = TFDist.fromVec(pmfVec ++ that.pmfVec)

  def *(sc: Output[Double]): TFDist[A] =
    TFDist(pmfMap.mapValues(w => tf.multiply(w, sc)))

  def map[B](f: A => B): TFDist[B] =
    TFDist.fromVec(
      pmfVec.map { case (x, p) => f(x) -> p }
    )

  def entropy: Output[Double] =
    tf.negate(
      pmfMap.values
        .map((p) => tf.multiply(p, tf.log(p)))
        .reduce[Output[Double]] {
          case (x, y) => tf.add(x, y)
        })

  def klDivergence(that: TFDist[A]): Output[Double] =
    pmfVec
      .map {
        case (x, p) => tf.multiply(p, tf.log(tf.divide(p, that.pmfMap(x))))
      }
      .reduce[Output[Double]] {
        case (x, y) => tf.add(x, y)
      }

  def getMap(session: Session) =
    pmfVec.map { case (x, p) => x -> session.run(fetches = p).scalar }.toMap

  def getFD(session: Session): FiniteDistribution[A] =
    FiniteDistribution(getMap(session).map {
      case (x, p) => Weighted(x, p)
    }.toVector)

}

object TFDist {
  def fromVec[A](v: Vector[(A, Output[Double])]): TFDist[A] = {
    val pmfMapVec: Map[A, Vector[Output[Double]]] =
      v.groupBy(_._1).mapValues((v) => v.map(_._2))
    val pmfMap: Map[A, Output[Double]] = pmfMapVec.mapValues(v =>
      v.reduce[Output[Double]] { case (x, y) => tf.add(x, y) })
    TFDist(pmfMap)
  }

  def fromFD[A](fd: FiniteDistribution[A]): TFDist[A] = {
    val pmfMap = fd.pmf.map {
      case Weighted(elem, weight) => elem -> tf.constant[Double](weight.toDouble)
    }.toMap
    TFDist(pmfMap)
  }
}

class TFEg {
  val x: api.tf.Variable[Double] =
    tf.variable[Double]("x", Shape(1, 1), tf.ZerosInitializer)
  val p: Output[Double] = tf.sigmoid(x)
  val dist: TFDist[Int] =
    (TFDist.fromFD(FiniteDistribution.unif(1, 2, 3, 4, 5, 6, 7, 8)) * p) ++
      (TFDist.fromFD(FiniteDistribution.unif(5)) * tf.subtract(1.0, p))

  val h: Output[Double] = dist.entropy

  val trainOp: UntypedOp = tf.train.AdaGrad(1.0f).minimize(tf.negate(h))

  val session = Session()

  session.run(targets = tf.globalVariablesInitializer())

  def tuned(steps: Int): FiniteDistribution[Int] = {

    (1 to steps).foreach { j =>
      //      println(j)
      val trainLoss = session.run(fetches = h, targets = trainOp)
      if (j % 100 == 0) println(s"loss: ${trainLoss.scalar}, steps: $j")
    }

    dist.getFD(session)

  }


}

case class EgUnif(n: Int) {
  val rnd = new scala.util.Random()

  val probVars: Vector[api.tf.Variable[Double]] = (0 to n)
    .map(j =>
      tf.variable[Double](s"p$j",
        Shape(1, 1),
        initializer = tf.ConstantInitializer[Double](5 * (rnd.nextDouble() - 0.5))))
    .toVector

  val totSig: Output[Double] =
    probVars.map(v => tf.sigmoid(v: Output[Double])).reduce[Output[Double]] {
      case (x, y) => tf.add(x, y)
    }

  val probs: Vector[Output[Double]] =
    probVars.map(v => tf.divide(tf.sigmoid(v), totSig))


  val dist =
    TFDist(
      (0 to n).map(j => j -> probs(j)).toMap
    )


  val loss: Output[Double] = tf.negate(dist.entropy)

  val trainOp: UntypedOp = tf.train.AdaGrad(1.0f).minimize(loss)

  val geomFD: FiniteDistribution[Int] = FiniteDistribution(
    (0 to n).toVector.map(j => Weighted(j, math.pow(2, -(j + 1))))
  ).normalized()

  val geomDist: TFDist[Int] = TFDist.fromFD(geomFD)

  val klLoss: Output[Double] = geomDist.klDivergence(dist)

  val klTrainOp: UntypedOp = tf.train.AdaGrad(1.0f).minimize(klLoss)

  val session = Session()

  session.run(targets = tf.globalVariablesInitializer())

  def tuned(steps: Int): FiniteDistribution[Int] = {

    (1 to steps).foreach { j =>
      //      println(j)
      val trainLoss = session.run(fetches = loss, targets = trainOp)
      if (j % 100 == 0) println(s"loss: ${trainLoss.scalar}, steps: $j")
    }


    dist.getFD(session)
  }

  def klTuned(steps: Int): FiniteDistribution[Int] = {

    (1 to steps).foreach { j =>
      //      println(j)
      val trainLoss = session.run(fetches = klLoss, targets = klTrainOp)
      if (j % 100 == 0) println(s"loss: ${trainLoss.scalar}, steps: $j")
    }


    dist.getFD(session)
  }


}
