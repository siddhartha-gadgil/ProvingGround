package provingground


import spire.math._
import spire.algebra._

import spire.implicits._

import Z3RealExpr._

import FreeGroups._

import com.microsoft.z3._

object SU2Representations {
  import ctx._

  import Z3RealField._

  /**
   * Quaternionic variable class, i.e., 4 real variables.
   */
  case class QuatVar(name: String){
    val r = realVar(s"$name.r")

    val i = realVar(s"$name.i")

    val j = realVar(s"$name.j")

    val k = realVar(s"$name.k")

    /**
     * Quaternion with components the variables
     */
    val quatVar = Quaternion(r, i, j, k)

    /**
     * norm square
     */
    val normsq = (r * r) + (i * i) + (j * j) + (k * k)

    /**
     * equation saying variables give a unit quaternion.
     */
    val isUnit = mkEq(normsq, one)

    /**
     * Equations saying variables give the quaternion 1.
     */
    val isId = List(mkEq(r, one), mkEq(i, zero), mkEq(j, zero), mkEq(k, zero))

    /**
     * quaternionic varaible is not 1
     */
    val isNotId = mkNot(isId reduce (mkAnd(_, _)))
  }

  /**
   * the unit quaternion.
   */
  val unity = Quaternion(one, zero, zero, zero)

  /**
   * Image of a word in quaternions, given image of generators.
   */
  def wordImage(gens: Seq[Quaternion[ArithExpr]])(w: Word) : Quaternion[ArithExpr] = w.ls match {
      case List() => unity
      case k :: ys =>
        if (k> 0) gens(k-1) * wordImage(gens)(Word(ys)) else gens(-k-1).conjugate * wordImage(gens)(Word(ys))
  }
  /**
   * List of equations for equality of quaternions.
   */
  def equality(x: Quaternion[ArithExpr], y: Quaternion[ArithExpr]) = {
    List(mkEq(x.r, y.r), mkEq(x.i, y.i), mkEq(x.j, y.j), mkEq(x.k, y.k))
  }

  /**
   * Equations for representations given a presentation.
   */
  case class PresentationEquations(p: Presentation){
    val vars = (for (j <- 0 until p.rank) yield QuatVar(('a'+j).toChar.toString)).toVector

    /**
     * Variables for generators
     */
    val gens = vars map (_.quatVar)

    /**
     * Images of relations
     */
    val relImages = for (rel <- p.rels) yield wordImage(gens)(rel)

    /**
     * equations for relations mapped to unity
     */
    val RelEqns = relImages flatMap (equality(_, unity))

    /**
     * equations for generators mapped to unit quaternions.
     */
    val GenEqns = vars map (_.isUnit)

    /**
     * equations for being a representation
     */
    val isRep = RelEqns ++ GenEqns

    /**
     * some generator is not mapped to unity
     */
    val isNonTriv = mkNot((vars flatMap (_.isId)) reduce (mkAnd(_, _)))
    }

  def hasNonTrivRepSolver(p: Presentation) = {
    val solver = mkSolver

    val pe = PresentationEquations(p)

    (pe.isRep) foreach (solver.add(_))

    solver.add(pe.isNonTriv)

    solver
  }

  def hasNonTrivRep(p: Presentation) = hasNonTrivRepSolver(p).check

}
