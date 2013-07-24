package provingGround

import scala.language.implicitConversions 

object HoTT{
  object Scala{
  /** A universe of types, with underlying scala types subtypes of A */
  trait SubTypOf[+A]{ 
    type typ <: A
  }
  
  /** A HoTT type with objects of this type having scala type A*/
  trait Typ[A] extends SubTypOf[A] with Expression{
    type typ = A
    
    def ->[B](codom: Typ[B]) = FuncTyp(this, codom)
    
    def +[B](that: Typ[B]) = SumTyp(this, that)
    
    def *[B](that: Typ[B]) = ProdTyp(this, that)
    
    def ::(a: A)= Obj(a, this)
    
    def ::(s: SymVar) = TypedVar(s, this)
  }
  
  trait VarExpression extends Expression{
    def -> (that: Expression) = Mapping(this, that)
  }
  
  case class SymVar(sym: Symbol) extends VarExpression
  
  implicit def symVar(sym: Symbol) = SymVar(sym)
  
  trait Expression{
    def apply(that: Expression) = Application(this, that)
    
    def +(that: Expression) = 'plus (this)(that)
    
    def -(that: Expression) = 'minus (this) (that)
    
    def *(that: Expression) = 'star (this) (that)
    
    def /(that: Expression) = 'slash (this) (that)
    
    def **(that: Expression) = 'expo (this) (that)
  }
  
  case class Application(f: Expression, t: Expression) extends Expression  
  
  case class Mapping(s: VarExpression, result: Expression) extends Expression
  
  case class IntExpr(value: Long) extends Expression
  
  implicit def intExpr(value: Long) = IntExpr(value)
  
  case class RealExpr(value: Double) extends Expression
  
  implicit def realExpr(value: Double) = RealExpr(value)  
  
  case class TypedVar[A](sym: SymVar, override val ty: Typ[A]) extends Obj[A](ty) with VarExpression
  
  /** An object with a specified HoTT type */
  class Obj[A](val ty: Typ[A]) extends Expression
  
  /** These are those with verified types */
  sealed class VerifiedObj[A](ty: Typ[A]) extends Obj[A](ty)
  
  trait Value[A]{
    def value: A
  }
  
  abstract class ValueObj[A](ty: Typ[A]) extends Obj[A](ty) with Value[A]
  
  abstract class VerifiedValueObj[A](ty: Typ[A]) extends Obj[A](ty) with Value[A]
  
  case class ValObj[A](value: A, override val ty: Typ[A]) extends ValueObj[A](ty)
  
  object Obj{
    def apply[A](a: A, t: Typ[A]) = ValObj(a, t)
      }
  
  
  
  /** A scala type from a HoTT type */
  case class TypeOf[A](a: A) extends Typ[A]
  
  /** The type of functions dom => codom */
  case class FuncTyp[A, B](dom: Typ[A], codom: Typ[B]) extends Typ[A => B]{
	override type typ = dom.typ => codom.typ
  }
  
  case class Func[A, B](value: A => B, dom: Typ[A], codom: Typ[B]) extends ValueObj[A => B](FuncTyp(dom, codom)){
    def apply(a: Obj[A]) = Apply(this, a)
  }
  
  case class Apply[A, B](func: Func[A, B], a: Obj[A]) extends Obj[B](func.codom)
  
  /** A type family, taking a base (underlying type A) to a family of types (all subtypes of V) */
  case class TypFamily[A, +V](base: Typ[A], fibers: A => SubTypOf[V])
  
  /** The product type for a type family */
  case class PiTyp[A, V](family: TypFamily[A,V]) extends Typ[A => V]
  
  /** A dependent function */
  case class DepFunc[A, V](section: A => V, family: TypFamily[A,V]) extends ValueObj[A =>V](PiTyp(family)){ 
    lazy val value = section
  }
  
  /** Sum type*/
  case class SigmaTyp[A, V](family: TypFamily[A,V]) extends Typ[(V, A)]
  
  case class InSum[A, V](obj: V, index: A, family: TypFamily[A, V])extends ValueObj[(V, A)](SigmaTyp(family)){
    lazy val value = (obj, index)
  }
  
  
  case class ProdTyp[A, B](a: Typ[A], b: Typ[B]) extends Typ[(A, B)]

  case class SumTyp[A,B](a: Typ[A], b: Typ[B]) extends Typ[Either[A,B]]
  
  case object ZeroTyp extends Typ[Nothing] 
  
  case class FromZero[A](a: Typ[A]) extends VerifiedValueObj[Nothing => A](FuncTyp[Nothing, A](ZeroTyp, a)){
    val value: Nothing => A = (x: Nothing) => (x: A)
  }
  
  case object OneTyp extends Typ[Unit]
  
  case object unit extends VerifiedValueObj[Unit](OneTyp){
    val value = {}
  }
  
  case object Nat extends Typ[Long]
  
  case object zero extends VerifiedValueObj[Long](Nat){
    val value: Long = 1
  }
  
  case object succ extends VerifiedValueObj[Long => Long](FuncTyp(Nat, Nat)){
    lazy val value = (n: Long) => n + 1 
  }
  
  case class IdentityTyp[A](typ: Typ[A], a: A, b: A) extends Typ[Boolean]
  
  def IdFamily[A](base: Typ[A])(ab: (A, A)) = IdentityTyp(base, ab._1, ab._2)
  
  def Identity[A](base: Typ[A]) = TypFamily(base * base, IdFamily(base))
  
  case class Refl[A](base: Typ[A], a: A) extends VerifiedValueObj[Boolean](IdentityTyp(base, a, a)){ 
    val value = true
  }
  
  def reflFamily[A](base: Typ[A]) = TypFamily(base, (a: A) => IdFamily(base)((a,a)))
  
  def refl[A, Boolean](base: Typ[A]) = DepFunc((a: A) => Refl(base, a), reflFamily(base))
  }
  
  
  
  
  
  
  
  object Pure{    
    trait AbsObj{
      val typ: Typ
      
      val freeVars: Set[Variable]
      
      def as(that: Typ) = {assert (typ==that); this.asInstanceOf[that.Obj]}
    }
       
    class NextUniv(u: Typ) extends Universe
    
    
    trait Variable
    
    trait Universe extends Typ{
      val myself = this
      trait Obj extends super.Obj with Typ{   
        override val typ: Typ = myself
        
        override val freeVars: Set[Variable] = Set.empty
       }
      
      
    }
    
    
    trait Typ extends AbsObj{
      val freeVars: Set[Variable] = Set.empty
      
      val typ: Typ  = new NextUniv(this)
      
      val self = this
      
      def -->(that: Typ) = FuncTyp(this, that)
      
      trait Obj extends AbsObj{
        val typ = self
        
        val freeVars: Set[Variable] = self.freeVars
        
        val id = Identity(this, this)
        
        case object refl extends id.Obj
      }
      
      case class TypedVar(sym: Symbol) extends Obj with Variable{
        override val freeVars: Set[Variable] = Set(this)
      }
      
      implicit def ::(sym: Symbol)= TypedVar(sym)
      
      def ::(obj: AbsObj) = obj.as(this)
      
      case class Identity(left: Obj, right: Obj) extends Typ{
        override val freeVars: Set[Variable] = self.freeVars
      }
      
      case class lambda(x: Variable)(result: Obj) extends Obj{
        override val freeVars = result.freeVars - x
      }      
      
    }
    
    implicit def me(arg: AbsObj): arg.typ.Obj = arg as arg.typ
    
    val i = Nat.lambda('x :: Nat)('x)
    
    def lambda(x: Variable)(res: AbsObj) = {
      val tp = res.typ
      tp.lambda(x)(res as tp)
    }
    
    def id(typ: Typ)(left: AbsObj, right: AbsObj) = typ.Identity(left as typ, right as typ) 
    
    implicit class IndexTyp(val tp: Typ){
      case class DepFuncTyp(section: tp.Obj => Typ) extends Typ{
        case class Index(arg: tp.Obj){
          val codomain = section(arg)
          case class Dappl(f: Obj) extends codomain.Obj{
            override val freeVars = f.freeVars ++ arg.freeVars
          }
          
          case class DepPair(value: codomain.Obj) extends Typ
        }
        
        trait Obj extends super.Obj{
          def act(arg: tp.Obj) = {
            val domain = Index(arg)
            (domain.Dappl)(this)
            }
          
          def apply(arg:tp.Obj) = act(arg)
          
          def apply(arg: AbsObj) = act(arg.as(tp))
        }
      }            
    }
    
    
 
    
    case class FuncTyp(dom: Typ, codom: Typ) extends Funcs
    
    case class TypFamily(dom: Typ, codom: Universe) extends Funcs
    
    trait Funcs extends Typ{
      val dom: Typ
      val codom: Typ
      
      override val freeVars = dom.freeVars ++ codom.freeVars
      
      case class Appl(f: Obj, arg: dom.Obj) extends codom.Obj{
        override val freeVars = arg.freeVars ++ f.freeVars
      }
      
      
      trait Obj extends super.Obj{
        def act(arg: dom.Obj) : codom.Obj = Appl(this, arg)
        
        def apply(arg: dom.Obj): codom.Obj  =  act(arg)
        
        def apply(arg: AbsObj): codom.Obj = act(arg.as(dom))
        
        case class Defn(defn: dom.Obj => codom.Obj) extends Obj{ 
          override def act(arg: dom.Obj) = defn(arg)
        }
      }
       
    }
     
    
    case object ZeroTyp extends Typ
    
    case object OneTyp extends Typ
    
    case object star extends OneTyp.Obj
    
    trait ConstructorDomain{
      val dom: Typ => Typ 
      
      def apply(that: Typ) = dom(that)
    }
    
    
    case class ConstContrDom(typ: Typ) extends ConstructorDomain{
      val dom = (that: Typ) => typ
    }
    
    case class ToThis(domdom: ConstructorDomain) extends ConstructorDomain{
      val dom = (that: Typ) =>  domdom.dom(that) --> that
    }
    
    case class InductiveTyp(constructors: Map[Symbol, ConstructorDomain]) extends Typ{
      val constrs = constructors map (_._2)
      
      def rec(that: Typ) = {}
    }
    

    
    case object Nat extends Typ{
      case class Rec(that: Typ){
        val fnTyp = FuncTyp(Nat, that)
        type domTyp = fnTyp.dom.Obj
        val tgt = Nat --> (that --> that)
        case class rec(base: that.Obj, step: tgt.Obj) extends fnTyp.Obj{
          val atZero = this(zero.as(fnTyp.dom)).as(that)
          val baseIdtyp = that.Identity(atZero, base)
          case object baseId extends baseIdtyp.Obj
        } 
        
        
      }
    }
    
    case object zero extends Nat.Obj 
    
    case class succ(n: Nat.Obj) extends Nat.Obj
    
    val one = succ(zero)
 
    def mk(a: Typ, b: Typ): a.Obj => b.Obj = {assert(a==b); (x:a.Obj) => x.asInstanceOf[b.Obj]}
    
  }
}