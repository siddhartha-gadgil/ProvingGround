package provingground.functionfinder

import provingground._, HoTT._

import ScalaRep._

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.util._
import scala.language.implicitConversions
import annotation.tailrec
//import scala.language.existentials

/**
  * @author gadgil
  */
// class SymbolicCRing[A: CRing] extends SymbolicCRig[A] { self =>
//   val ring = implicitly[Ring[A]]
//
//   import ring._
//
//   val minusone = Literal(ring.negate(one))
//
//   val negate = prod(minusone)
//
//   implicit val cringStructure: CRing[LocalTerm] = new CRing[LocalTerm] {
//     val zero = Literal(ring.zero)
//
//     val one = Literal(ring.one)
//
//     def plus(x: LocalTerm, y: LocalTerm) = self.sum(x)(y)
//
//     def times(x: LocalTerm, y: LocalTerm) = self.prod(x)(y)
//
//     def negate(x: LocalTerm) = self.negate(x)
//   }
// }
//
//
