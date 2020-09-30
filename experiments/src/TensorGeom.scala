package provingground.experiments


import org.platanios.tensorflow.api._

import org.platanios.tensorflow.api.implicits.helpers._

class TensorGeom(p: Float = 0.5.toFloat, N: Int = 10) {
  lazy val q: Output[Float] = Tensor(1.0.toFloat - p)

  lazy val prob = (0 until N).toVector.map { (j) =>
    tf.variable[Float](s"p$j", Shape(1, 1), tf.ZerosInitializer)
  }

  lazy val probOut: Seq[Output[Any]] = prob.map((x) => x: Output[Float])

  lazy val totErr: Output[Float] =
    tf.square(prob.foldLeft[Output[Float]](Tensor(-1.0.toFloat))(_ + _))

  def recEqn(n: Int) = tf.square((q * prob(n - 1)) - prob(n))

  lazy val loss = (1 until N).map(recEqn(_)).foldLeft(totErr)(_ + _)

  lazy val trainOp = tf.train.AdaGrad(1.0.toFloat).minimize(loss)

  lazy val session = Session()

  def result = {
    session.run(targets = tf.globalVariablesInitializer())

    session.run(fetches = probOut.toSeq, targets = trainOp)
  }
}
