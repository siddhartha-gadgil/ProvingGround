package provingground

import HoTT._

import ScalaRep._

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.util._
import scala.language.implicitConversions
import annotation.tailrec

object NatRing extends SymbolicCRing[SafeLong]{
  override def toString = "Nat"
  val x = "x" :: LocalTyp
  val succ = lmbda(x)(x + 1)

  val NatTyp = LocalTyp

  type Nat = LocalTerm


  @tailrec def recDefn[U <: Term with Subs[U]](n: SafeLong, formal: U, h: SafeLong => U => U) : U =
    if (n == 0) formal else recDefn(n -1 , h(n)(formal), h)

    case class Rec[U <: Term with Subs[U]](init: U, g: Func[Nat, Func[U, U]]) extends Func[Nat, U]{self =>
        def h = (n: SafeLong) => g(Literal(n))

        val dom = NatTyp
        val codom = init.typ.asInstanceOf[Typ[U]]

        val typ = dom ->: codom

        def subs(x: Term, y: Term) = this
        def newobj = this


        def act(x: LocalTerm) = x match {
          case Literal(n) => recDefn(n, init, h)
          case LiteralSum(n, x) => recDefn(n, Rec(init, g)(x), (n: SafeLong) => g(Literal(n)))
          case _ => FormalAppln[Nat, U](self, x)
        }

    }
  
   case class Induc[U <: Term with Subs[U]](
       typFamily: Func[Nat, Typ[U]], init: U, g: FuncLike[Nat, Func[U, U]]) extends FuncLike[Nat, U]{self =>
        def h = (n: SafeLong) => g(Literal(n))

        val dom = NatTyp

        val typ = PiTyp(typFamily)
        
        val depcodom = typFamily

        def subs(x: Term, y: Term) = this
        def newobj = this


        def act(x: LocalTerm) = x match {
          case Literal(n) => recDefn(n, init, h)
          case LiteralSum(n, x) => recDefn(n, Induc(typFamily, init, g)(x), (n: SafeLong) => g(Literal(n)))
          case _ => FormalAppln[Nat, U](self, x)
        }

    }
   
   object Induc{
     def cast[U <: Term with Subs[U]](
       typFamily: Func[Nat, Typ[U]], init: U, g: FuncLike[Nat, FuncLike[U, U]]) = 
         Induc(typFamily, init, g.asInstanceOf[FuncLike[Nat, Func[U, U]]])
     
   }
}