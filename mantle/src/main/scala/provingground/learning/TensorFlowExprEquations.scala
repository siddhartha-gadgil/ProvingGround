package provingground.learning

import provingground._, HoTT._

import ExpressionEval._, ExprCalc._, ExprEquations._
import GeneratorVariables._, TermRandomVars._
import org.tensorflow._, org.tensorflow.op._, types._
import org.tensorflow.framework.optimizers.GradientDescent
import scala.util.{Using, Try}
import scala.jdk.CollectionConverters._
import org.tensorflow.op.linalg.MatMul
object TensorFlowExprEquations {
  def opLookup(v: Operand[TFloat32], sess: Session): Float = {
    val result = sess.runner().fetch(v).run()
    val data   = result.get(0).expect(TFloat32.DTYPE).data()
    data.getFloat()
  }

  def equationEvolver(
      graph: Graph,
      initState: TermState,
      equationSet: Set[Equation],
      params: TermGenParams,
      learningRate: Float
  ) = {
    new TensorFlowExprEquations(
      ExpressionEval
        .initMap(ExpressionEval.eqAtoms(equationSet), params, initState),
      equationSet,
      params,
      graph,
      learningRate
    )
  }

  def takeSteps(
      initState: TermState,
      equationSet: Set[Equation],
      params: TermGenParams,
      steps: Int,
      learningRate: Float = 0.1f
  ): Try[FiniteDistribution[HoTT.Term]] =
    Using(new Graph()) { graph =>
      val evolver =
        equationEvolver(graph, initState, equationSet, params, learningRate)
      evolver.fit(steps)
    }.flatten

  def quickCheck(
      initState: TermState,
      equationSet: Set[Equation],
      params: TermGenParams,
      learningRate: Float = 0.1f
  ) = Using(new Graph()) { graph =>
    val evolver =
      equationEvolver(graph, initState, equationSet, params, learningRate)
    evolver.quickCheck()
  }
}

import TensorFlowExprEquations._

class TensorFlowExprEquations(
    initMap: Map[Expression, Double], // values specified and frozen
    equationSet: Set[Equation],
    params: TermGenParams,
    graph: Graph,
    learningRate: Float,
    initVariables: Vector[Expression] = Vector() // values that can evolve
) extends ExprEquations(initMap, equationSet, params, initVariables) {
  val numVars = size + initVariables.size
  val tf      = Ops.create(graph)
  val xs = (0 until (numVars)).toVector
    .map(j => tf.withName(s"x$j").variable(tf.constant(0f)))
  val ps: Vector[Operand[TFloat32]] = xs.map(
    x => tf.math.sigmoid(x)
  )

  def prodOp(prod: ProdExpr): Option[Operand[TFloat32]] = {
    if (prod.constant == 0) None
    else Some{
      val num: Operand[TFloat32] = prod.indices
        .map(ps(_))
        .fold[Operand[TFloat32]](tf.constant(prod.constant.toFloat))(
          tf.math.mul(_, _)
        )
      prod.negIndices
        .map(n => tf.math.reciprocal(ps(n)))
        .fold[Operand[TFloat32]](num)(tf.math.mul(_, _))
    }
  }

  def sumOp(sum: SumExpr): Option[Operand[TFloat32]] = {
    val terms = sum.terms
      .flatMap(prodOp(_))
    // if (terms.isEmpty) println("all product terms with 0 coefficients") else println("Got a sum")
    if (terms.isEmpty) None else Some(terms.reduce[Operand[TFloat32]](tf.math.add(_, _)))
  }

  def equationsLogMismatch(
      eqs: Vector[(Operand[TFloat32], Operand[TFloat32])]
  ): Operand[TFloat32] = {
    eqs
      .map {
        case (lhs, rhs) =>
          tf.math.squaredDifference(tf.math.log(lhs), tf.math.log(rhs))
      }
      .reduce[Operand[TFloat32]](
        tf.math.add(_, _)
      )
  }

  def equationsRatioMismatch(
      eqs: Vector[(Operand[TFloat32], Operand[TFloat32])]
  ): Operand[TFloat32] = {
    eqs
      .map {
        case (lhs, rhs) =>
          tf.math.div(
            tf.math.abs(tf.math.sub(lhs, rhs)),
            tf.math.add(lhs, rhs)
          )
      }
      .reduce[Operand[TFloat32]](
        tf.math.add(_, _)
      )
  }

  val matchEquationsOp: Vector[(Operand[TFloat32], Operand[TFloat32])] =
    rhsExprs.zipWithIndex.map {
      case (rhs, n) => 
        // if (sumOp(rhs).isEmpty) println(s"Zero rhs in ${equationVec(n)}")
        (ps(n),  sumOp(rhs).getOrElse(tf.constant(0f)) )
    }

  val totalProbEquationsOp: Vector[(Operand[TFloat32], Operand[TFloat32])] =
    randomVarIndices
      .to(Vector)
      .map { gp =>
        (gp.map(ps(_)).reduce(tf.math.add(_, _)), tf.constant(1f))
      }

  val mismatch: Operand[TFloat32] = equationsRatioMismatch(
    matchEquationsOp ++ totalProbEquationsOp
  )

  def termProbs(session: Session) = termIndexVec.map {
    case (t, n) => t -> opLookup(ps(n), session)
  }

  def termDist(session: Session) =
    FiniteDistribution(termProbs(session).map { case (x, p) => Weighted(x, p) })

  val optimizer = new GradientDescent(graph, learningRate)

  val shift = optimizer.minimize(mismatch)

  def quickCheck(): Try[Tensor[TFloat32]] =
    Using(new Session(graph)) { session =>
      session.run(tf.init())
      val output = session.runner().fetch(mismatch).run()
      output.get(0).expect(TFloat32.DTYPE)
    }

  def fit(steps: Int): Try[FiniteDistribution[HoTT.Term]] =
    Using(new Session(graph)) { session =>
      println("Running steps")
      session.run(tf.init())
      (0 until steps).foreach { j =>
        // if (j %10 == 0)
        println(s"Ran $j steps")
        session.run(shift)
      }
      println("getting terms")
      termDist(session)
    }
}
