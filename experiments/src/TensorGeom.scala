package provingground.experiments

/*
import org.platanios.tensorflow.api._

class TensorGeom(p: Double = 0.5, N: Int = 10) {
  lazy val q: Output = Tensor(1.0 - p)

  lazy val prob = (0 until N).toVector.map { (j) =>
    tf.variable(s"p$j", FLOAT32, Shape(1, 1), tf.ZerosInitializer)
  }

  lazy val probOut: Vector[Output] = prob.map((x) => x: Output)

  lazy val totErr: Output =
    tf.square(prob.foldLeft[Output](Tensor(-1.0))(_ + _))

  def recEqn(n: Int) = tf.square((q * prob(n - 1)) - prob(n))

  lazy val loss = (1 until N).map(recEqn(_)).foldLeft(totErr)(_ + _)

  lazy val trainOp = tf.train.AdaGrad(1.0).minimize(loss)

  lazy val session = Session()

  def result = {

    session.run(targets = tf.globalVariablesInitializer())

    session.run(fetches = probOut, targets = trainOp)
  }
}
*/