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
  
  case class QuatVar(name: String){
    val r = realVar(s"$name.r")
    
    val i = realVar(s"$name.i")
    
    val j = realVar(s"$name.j")
    
    val k = realVar(s"$name.k")
    
    val quatVar = Quaternion(r, i, j, k)
    
//    val  f= implicitly[Field[ArithExpr]]
    
    val normsq = (r * r) + (i * i) + (j * j) + (k * k)
    
    val isUnit = mkEq(normsq, one)
    
    val isId = List(mkEq(r, one), mkEq(i, zero), mkEq(j, zero), mkEq(k, zero))
    
    val isNotId = mkNot(isId reduce (mkAnd(_, _)))
  }
  
  val unity = Quaternion(one, zero, zero, zero)
 
  
  def wordImage(gens: Seq[Quaternion[ArithExpr]])(w: Word) : Quaternion[ArithExpr] = w.ls match {
      case List() => unity
      case k :: ys => 
        if (k> 0) gens(k) * wordImage(gens)(Word(ys)) else gens(-k).conjugate * wordImage(gens)(Word(ys))
  }
  
  def equality(x: Quaternion[ArithExpr], y: Quaternion[ArithExpr]) = {
    List(mkEq(x.r, y.r), mkEq(x.i, y.i), mkEq(x.j, y.j), mkEq(x.k, y.k))
  }
  
  case class PresentationEquations(p: Presentation){
    val vars = (for (j <- 0 until p.rank) yield QuatVar(('a'+j).toChar.toString)).toVector
    
    val gens = vars map (_.quatVar)
    
    val relImages = for (rel <- p.rels) yield wordImage(gens)(rel)
    
    val RelEqns = relImages flatMap (equality(_, unity))
    
    val GenEqns = vars map (_.isUnit)
    
    val repEqns = RelEqns ++ GenEqns
    
    val isPresentation = (RelEqns ++ GenEqns) 
    
    
    val isNonTrivPresentation = mkNot((vars flatMap (_.isId)) reduce (mkAnd(_, _)))
    }

  def checkNonTrivPres(p: Presentation) = {
    val solver = mkSolver
    
    val pe = PresentationEquations(p)
    
    (pe.repEqns) foreach (solver.add(_))
    
    solver.add(pe.isNonTrivPresentation)
    
    solver.check()
  }
  
}