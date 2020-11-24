package provingground.learning

import provingground._, HoTT._

import ExpressionEval._, ExprCalc._, ExprEquations._
import GeneratorVariables._, TermRandomVars._
import org.tensorflow._, org.tensorflow.op._, types._
import org.tensorflow.framework.optimizers.GradientDescent
import scala.util.Using

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
      params: TermGenParams
  ) = {
    new TensorFlowExprEquations(
      ExpressionEval
        .initMap(ExpressionEval.eqAtoms(equationSet), params, initState),
      equationSet,
      params,
      graph
    )
  }

  def takeSteps(
      initState: TermState,
      equationSet: Set[Equation],
      params: TermGenParams,
      steps: Int,
      learningRate: Float = 0.1f
  ) =
    Using.Manager { use =>
      val graph   = use(new Graph())
      println("Seeking evolver")
      val evolver = equationEvolver(graph, initState, equationSet, params)
      println("Got evolver")
      val session = use(new Session(graph))
      val gd      = evolver.gradientDescent(learningRate)
      session.run(evolver.tf.init())
      val loss  = evolver.mismatch
      val shift = gd.minimize(loss)
      println("Running steps")
      (0 until steps).foreach{j => 
        // if (j %10 == 0)
         println(s"Ran $j steps")
        session.run(shift)}
      evolver.termDist(session)
    }
}

import TensorFlowExprEquations._

class TensorFlowExprEquations(
    initMap: Map[Expression, Double], // values specified and frozen
    equationSet: Set[Equation],
    params: TermGenParams,
    graph: Graph,
    initVariables: Vector[Expression] = Vector() // values that can evolve
) extends ExprEquations(initMap, equationSet, params, initVariables) {
  val numVars = size + initVariables.size
  val tf      = Ops.create(graph)
  val xs = (0 until (numVars)).toVector
    .map(j => tf.withName(s"x$j").variable(tf.constant(0f)))
  val ps: Vector[Operand[TFloat32]] = xs.map(
    x => //x
      tf.math.sigmoid(x)
  )

  def prodOp(prod: ProdExpr): Operand[TFloat32] = {
    val num: Operand[TFloat32] = prod.indices
      .map(ps(_))
      .fold[Operand[TFloat32]](tf.constant(prod.constant.toFloat))(
        tf.math.mul(_, _)
      )
    prod.negIndices
      .map(n => tf.math.reciprocal(ps(n)))
      .fold[Operand[TFloat32]](num)(tf.math.mul(_, _))
  }

  def sumOp(sum: SumExpr): Operand[TFloat32] =
    sum.terms
      .map(prodOp(_))
      .fold[Operand[TFloat32]](tf.constant(0f))(tf.math.add(_, _))

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

  val matchEquationsOp: Vector[(Operand[TFloat32], Operand[TFloat32])] =
    rhsExprs.zipWithIndex.map {
      case (rhs, n) => (ps(n), sumOp(rhs))
    }

  val totalProbEquationsOp: Vector[(Operand[TFloat32], Operand[TFloat32])] =
    randomVarIndices
      .to(Vector)
      .map { gp =>
        (gp.map(ps(_)).reduce(tf.math.add(_, _)), tf.constant(1f))
      }

  val mismatch: Operand[TFloat32] = equationsLogMismatch(
    matchEquationsOp ++ totalProbEquationsOp
  )

  def termProbs(session: Session) = termIndexVec.map {
    case (t, n) => t -> opLookup(ps(n), session)
  }

  def termDist(session: Session) =
    FiniteDistribution(termProbs(session).map { case (x, p) => Weighted(x, p) })

  def gradientDescent(l: Float) = new GradientDescent(graph, l)
}
