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
    
    val head = power(elems.head._1, elems.head._2) 
    
    val tail = if (elems.tail.isEmpty) Literal(one) else PiTerm(elems.tail)
    
    val composite = (elems.size > 1)
    
    def *:(y: Term) = {
      val ind = (elems.get(y) map (_+1)) getOrElse (1)
      PiTerm(elems + (y -> ind))
    }
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
      case y => AddTerm(y)
    }
  }
  
  case class AddTerm(x: Term) extends Func[Term, Term]{
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
  
  case class AdditiveMorphism[U<: Term with Subs[U]](base: Func[Term, U], op: (U, U) => U) extends Func[Term, Term]{
    val dom = Typ
    
    val codom = base.codom
    
    val typ = Typ ->: codom
    
    def subs(x: Term, y: Term) = AdditiveMorphism(base.subs(x, y), op)
    
    def newobj = AdditiveMorphism(base.newobj, op)
    
    def act(x: Term) = x match {
      case Comb(f, u, v) if f == sum => op(base(u), base(v))
      case SigmaTerm(elems) => (elems map ((u) => base(u))).reduce(op)
      case _ => base(x)
    }
  }
  
  @annotation.tailrec 
  final def power(x: Term, n: Int, accum: Term = Literal(one)): Term = {
    if (n ==0) Literal(one)
    else 
      if (n == 1) x
      else power(x, n-1, prod(accum)(x))
  }
  
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
          AdditiveMorphism(((b: A) => a * b).term, (x: Term, y: Term) => sum(x)(y))
      case Comb(op, u, v) if op == prod =>
        composition(prod(u), prod(v))
      case Comb(op, u, v) if op == sum =>
        funcSum(prod(u), prod(v))
      case s @ SigmaTerm(terms) => 
        (terms map ((t) => prod(t))).reduce(funcSum)
      case p: PiTerm =>  
        if (p.composite) composition(prod(p.head), prod(p.tail))
        else prod(p.head)
      case y => multTerm(y)
    }
  }
  
  
  case class multTerm(x: Term) extends Func[Term, Term]{
    val dom = Typ
    
    val codom = Typ
    
    val typ = Typ ->: Typ
    
    def subs(x: Term, y: Term) = this
    
    def newobj = this
    
    def act(y: Term) = y match{
      case Literal(a) => prod(Literal(a))(x)
      case Comb(f, Literal(a), v) if f == prod => prod(Literal(a))(prod(x)(v))
      case Comb(f, u, v) if f == sum => sum(prod(x)(u))(prod(x)(v))
      case SigmaTerm(elems) => (elems map ((u) => prod(x)(u))).reduce((a: Term, b: Term) => sum(a)(b))
      case p : PiTerm => x *: p
      case `x` => PiTerm(Map(x -> 2))
      case _ => PiTerm(Map(x -> 1, y -> 1))
    }
  }
  
}