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
class NumericTyp[A : CRig] {
  val rig = implicitly[CRig[A]]
  
  import rig._
  
  trait LocalTerm extends Term with Subs[LocalTerm]
  
  object LocalTyp extends Typ[LocalTerm]{
    type Obj = LocalTerm

    val typ = Universe(0)

    def symbObj(name: AnySym): LocalTerm = LocalSymbObj(name, this)

    def newobj = this

    def subs(x: Term, y: Term) = (x, y) match {
      case (xt: Typ[_], yt: Typ[_]) if (xt == this) => yt.asInstanceOf[Typ[LocalTerm]]
      case _ => this
    }
    
    implicit val rep : ScalaRep[LocalTerm, A] = SimpleRep(this)
  }
  
  case class LocalSymbObj[+U <: LocalTerm](name: AnySym, typ: Typ[U]) extends LocalTerm with Symbolic {
    override def toString = name.toString + " : (" + typ.toString + ")"

    def newobj = LocalSymbObj(new InnerSym(this), typ)

    def subs(x: Term, y: Term) = if (x == this) y.asInstanceOf[LocalTerm] else {
      def symbobj(sym: AnySym) = typ.replace(x, y).symbObj(sym)
      symSubs(symbobj)(x, y)(name)
    }
  }
  
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

  
  object Literal extends ScalaSym[LocalTerm, A](LocalTyp)

  object Comb{
    def unapply(term: LocalTerm): Option[(LocalTerm, LocalTerm, LocalTerm)] = term match {
      case FormalAppln(FormalAppln(op : LocalTerm, x : LocalTerm), y : LocalTerm) => Some((op, x, y))
      case _ => None
    }
    
    def apply(op: Func[LocalTerm, Func[LocalTerm, LocalTerm]], x: LocalTerm, y: LocalTerm) =
      FormalAppln(FormalAppln(op, x), y)
  }
  
  case class SigmaTerm(elems: Set[LocalTerm]) extends LocalTerm{
    val typ = LocalTyp
    
    def subs(x: Term, y: Term) = (elems map (_.subs(x, y))).reduce((a: LocalTerm, b: LocalTerm) => sum(a)(b))
    
    def newobj = LocalTyp.obj
    
    val head = elems.head
    
    val tail = 
      if (elems.size == 2) elems.last else SigmaTerm(elems.tail)
    
    /**
     * add a term, simplifies only in cases that remain in definition of sum
     */
    def +:(x: LocalTerm) : LocalTerm = {
        val l = LitProd.fold(x)(elems.toList)
        l match {
          case List() => Literal(zero)
          case s :: List() => s
          case _ => SigmaTerm(l.toSet)
        }
      }
    
  }
  
  object LitProd{
    def apply(a: A, x: LocalTerm) = prod(Literal(a))(x)
    
    def unapply(x: LocalTerm) : Option[(A, LocalTerm)] = x match {
      case Comb(mult, Literal(a), y) if mult == prod =>
        Some((a, y))
      case _ => None
    }
    
    def add(x: LocalTerm, y: LocalTerm) = (x, y) match {
      case (LitProd(a, u), LitProd(b, v)) if u == v =>
        Some(LitProd(a + b, u))
      case _ => None
    }
    
    def fold(x: LocalTerm)(l: List[LocalTerm]) : List[LocalTerm] = (x, l) match {
      case (_, List()) => List(x)
      case (_, head:: tail) =>
        (add(x, head) map ((u: LocalTerm) => fold(u)(tail))).
        getOrElse(head :: fold(x)(tail))
    }
  }
  
  
  case class PiTerm(elems: Map[LocalTerm, Int]) extends LocalTerm{
    val typ = LocalTyp
    
    def subs(x: Term, y: Term) = 
      (elems map ((an) => power(an._1, an._2))).reduce((a: LocalTerm, b: LocalTerm) => prod(a)(b))
    
    def newobj = LocalTyp.obj
    
    val head = power(elems.head._1, elems.head._2) 
    
    val tail = if (elems.tail.isEmpty) Literal(one) else PiTerm(elems.tail)
    
    val composite = (elems.size > 1)
    
    def *:(y: LocalTerm) = {
      val ind = (elems.get(y) map (_+1)) getOrElse (1)
      PiTerm(elems + (y -> ind))
    }
  }
  
  import LocalTyp.rep
  
  object sum extends Func[LocalTerm, Func[LocalTerm, LocalTerm]]{
    val dom = LocalTyp
    
    val codom = LocalTyp ->: LocalTyp
    
    val typ = dom ->: codom

    def subs(x: Term, y: Term) = this
    
    def newobj = this
    
    def act(x: LocalTerm) = x match {
      case Literal(a) => 
        if (a == zero)
        {
          val x = LocalTyp.obj
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
  
  case class AddTerm(x: LocalTerm) extends Func[LocalTerm, LocalTerm]{
    val dom = LocalTyp
    
    val codom = LocalTyp
    
    val typ = LocalTyp ->: LocalTyp
    
    def subs(x: Term, y: Term) = this
    
    def newobj = this
    
    def act(y: LocalTerm) = y match{
      case Literal(a) => Comb(sum, Literal(a), x)
      case Comb(f, Literal(a), v) if f == sum => sum(Literal(a))(sum(x)(v))
      case s : SigmaTerm => x +: s
      case _ => LitProd.add(x, y) getOrElse(SigmaTerm(Set(x, y)))
    }
  }
  
  def funcSum(f: LocalTerm => LocalTerm, g: LocalTerm => LocalTerm) = {
    val x = LocalTyp.obj
    lmbda(x)(sum(f(x))(g(x)))
  }
  
  case class AdditiveMorphism[U<: LocalTerm with Subs[U]](base: Func[LocalTerm, U], op: (U, U) => U) extends Func[LocalTerm, LocalTerm]{
    val dom = LocalTyp
    
    val codom = base.codom
    
    val typ = LocalTyp ->: codom
    
    def subs(x: Term, y: Term) = AdditiveMorphism(base.subs(x, y), op)
    
    def newobj = AdditiveMorphism(base.newobj, op)
    
    def act(x: LocalTerm) = x match {
      case Comb(f, u, v) if f == sum => op(base(u), base(v))
      case SigmaTerm(elems) => (elems map ((u) => base(u))).reduce(op)
      case _ => base(x)
    }
  }
  
  @annotation.tailrec 
  final def power(x: LocalTerm, n: Int, accum: LocalTerm = Literal(one)): LocalTerm = {
    if (n ==0) Literal(one)
    else 
      if (n == 1) x
      else power(x, n-1, prod(accum)(x))
  }
  
  object prod extends Func[LocalTerm, Func[LocalTerm, LocalTerm]]{
    val dom = LocalTyp
    
    val codom = LocalTyp ->: LocalTyp
    
    val typ = dom ->: codom

    def subs(x: Term, y: Term) = this
    
    def newobj = this
    
    def act(x: LocalTerm) = x match {
      case Literal(a) => 
        if (a == one)
        {
          val x = LocalTyp.obj
          lmbda(x)(x)
        }
        else
          AdditiveMorphism(((b: A) => a * b).term, (x: LocalTerm, y: LocalTerm) => sum(x)(y))
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
  
  
  case class multTerm(x: LocalTerm) extends Func[LocalTerm, LocalTerm]{
    val dom = LocalTyp
    
    val codom = LocalTyp
    
    val typ = LocalTyp ->: LocalTyp
    
    def subs(x: Term, y: Term) = this
    
    def newobj = this
    
    def act(y: LocalTerm) = y match{
      case Literal(a) => prod(Literal(a))(x)
      case Comb(f, Literal(a), v) if f == prod => prod(Literal(a))(prod(x)(v))
      case Comb(f, u, v) if f == sum => sum(prod(x)(u))(prod(x)(v))
      case SigmaTerm(elems) => (elems map ((u) => prod(x)(u))).reduce((a: LocalTerm, b: LocalTerm) => sum(a)(b))
      case p : PiTerm => x *: p
      case `x` => PiTerm(Map(x -> 2))
      case _ => PiTerm(Map(x -> 1, y -> 1))
    }
  }
  
}