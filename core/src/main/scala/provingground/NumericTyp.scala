package provingground

import HoTT._

import ScalaRep._

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.util._
import scala.language.implicitConversions

/**
 * @author gadgil
 */
class NumericTyp[A : Rig] {
  val rig = implicitly[Rig[A]]
  
  import rig._
  
  object Typ extends ScalaTyp[A]
  
  sealed trait Op{
    val unit : A
    
    def op(x: A, y: A): A
    
  }
  
  case object Plus extends Op{
    val unit = zero
    
    def op(x: A, y: A) = x + y
    
  }
  
  case object Times extends Op{
    val unit = one
    
    def op(x: A, y: A) = x * y
    
  }

  
  object Literal extends ScalaSym[Term, A](Typ)

  object Comb{
    def unapply(term: Term): Option[(Term, Term, Term)] = term match {
      case FormalAppln(FormalAppln(op, x), y) => Some((op, x, y))
      case _ => None
    }
    
    def apply(op: Func[Term, Func[Term, Term]], x: Term, y: Term) =
      FormalAppln(FormalAppln(op, x), y)
  }
  
  case class SigmaTerm(elems: Set[Term]) extends Term{
    val typ = Typ
    
    def subs(x: Term, y: Term) = ???
    
    def newobj = Typ.obj
    
    val head = elems.head
    
    val tail = 
      if (elems.size == 2) elems.last else SigmaTerm(elems.tail)
    
    /**
     * add a term, simplifies only in cases that remain in definition of sum
     */
    def +:(x: Term) : Term = {
        val l = LitProd.fold(x)(elems.toList)
        l match {
          case List() => Literal(zero)
          case s :: List() => s
          case _ => SigmaTerm(l.toSet)
        }
      }
    
  }
  
  object LitProd{
    def apply(a: A, x: Term) = prod(Literal(a))(x)
    
    def unapply(x: Term) : Option[(A, Term)] = x match {
      case Comb(mult, Literal(a), y) if mult == prod =>
        Some((a, y))
      case _ => None
    }
    
    def add(x: Term, y: Term) = (x, y) match {
      case (LitProd(a, u), LitProd(b, v)) if u == v =>
        Some(LitProd(a + b, u))
      case _ => None
    }
    
    def fold(x: Term)(l: List[Term]) : List[Term] = (x, l) match {
      case (_, List()) => List(x)
      case (_, head:: tail) =>
        (add(x, head) map ((u: Term) => fold(u)(tail))).
        getOrElse(head :: fold(x)(tail))
    }
  }
  
  
  case class PiTerm(elems: Map[Term, Int]) extends Term{
    val typ = Typ
    
    def subs(x: Term, y: Term) = ???
    
    def newobj = Typ.obj
    
    val head = natpow(elems.head._2)(elems.head._1) 
    
    val tail = if (elems.tail.isEmpty) one else PiTerm(elems.tail)
  }
  
  import Typ.rep
  
  object sum extends Func[Term, Func[Term, Term]]{
    val dom = Typ
    
    val codom = Typ ->: Typ
    
    val typ = dom ->: codom

    def subs(x: Term, y: Term) = this
    
    def newobj = this
    
    def act(x: Term) = x match {
      case Literal(a) => 
        if (a == zero)
        {
          val x = Typ.obj
          lmbda(x)(x)
        }
        else
          ((b: A) => a + b).term
      case Comb(op, u, v) if op == sum =>
        composition(sum(u), sum(v))
      case s @ SigmaTerm(terms) => 
        composition(sum(s.head), sum(s.tail))
      case y => addTerm(y)
    }
  }
  
  case class addTerm(x: Term) extends Func[Term, Term]{
    val dom = Typ
    
    val codom = Typ
    
    val typ = Typ ->: Typ
    
    def subs(x: Term, y: Term) = this
    
    def newobj = this
    
    def act(y: Term) = y match{
      case Literal(a) => Comb(sum, Literal(a), x)
      case Comb(f, Literal(a), v) if f == sum => sum(Literal(a))(sum(x)(v))
      case s : SigmaTerm => x +: s
      case _ => LitProd.add(x, y) getOrElse(SigmaTerm(Set(x, y)))
    }
  }
  
  def funcSum(f: Term => Term, g: Term => Term) = {
    val x = Typ.obj
    lmbda(x)(sum(f(x))(g(x)))
  }
  
  def natpow(n: Int) = ((a: A) => pow(a, n)).term
  
  object prod extends Func[Term, Func[Term, Term]]{
    val dom = Typ
    
    val codom = Typ ->: Typ
    
    val typ = dom ->: codom

    def subs(x: Term, y: Term) = this
    
    def newobj = this
    
    def act(x: Term) = x match {
      case Literal(a) => 
        if (a == one)
        {
          val x = Typ.obj
          lmbda(x)(x)
        }
        else
          ((b: A) => a * b).term
      case Comb(op, u, v) if op == prod =>
        composition(prod(u), prod(v))
      case Comb(op, u, v) if op == sum =>
        funcSum(prod(u), prod(v))
      case s @ SigmaTerm(terms) => 
        (terms map ((t) => prod(t))).reduce(funcSum)
      case y => ???
    }
  }
  
}