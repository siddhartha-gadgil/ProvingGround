package provingGround

import scala.language.implicitConversions 
import scala.util._

object HoTT{
  
   
    trait AbsObj extends WithFormal[AbsObj]{
      val typ: Typ[AbsObj]
      
      val freeVars: Set[Variable]
      
      def as[U <: WithFormal[U] with AbsObj](that: Typ[U]) = {assert (typ==that); this.asInstanceOf[that.Obj]}

			val FormalObj: AbsObj = FormalElem

			def formalElem[AbsObj] = FormalObj.asInstanceOf[AbsObj]
     }
         

    trait Base[+A]{def elem: A}
    
    implicit def baseElem[A](b: Base[A]): A = b.elem
    
    trait Variable
    
		

    case object FormalElem  extends AbsObj{
			val freeVars: Set[Variable] = Set.empty
			
			lazy val typ = FormalElemTyp
				}
    
		case object FormalElemTyp extends ElemTyp 

		trait WithFormal[+A<: AbsObj]{
			def formalElem[A] : A
		}

		 

    trait ElemTyp extends Typ[AbsObj]{
    	def asU(obj: Obj): AbsObj = obj
    	
    	val typ: Typ[AbsObj] = FirstUniv

			override def mkelem(ob: Obj): AbsObj = ob
    }


     
    
    object FirstUniv extends Typ[Typ[AbsObj]]{
      case class Tp(tp: Typ[AbsObj]) extends Obj{
        override def elem: Typ[AbsObj] = tp
      }
      
			case class TypVar(sym: Symbol) extends TypedVarTrait with Typ[AbsObj]{
				override val freeVars: Set[Variable] = Set(this)
				override val elem: Typ[AbsObj] = this
			}

      lazy val typ = NextUniv[Typ[AbsObj]]
			
			implicit override def ::(sym: Symbol): TypedVarTrait = TypVar(sym)
    }
    
	case class NextUniv[U <: WithFormal[U] with AbsObj]() extends Typ[Typ[U]]{
      case class Tp(tp: Typ[U]) extends Obj{
        override def elem: Typ[U] = tp
      }

			case class TypVar(sym: Symbol) extends TypedVarTrait with Typ[U]{
				override val freeVars: Set[Variable] = Set(this)
				override val elem: Typ[U] = this
			}

			implicit override def ::(sym: Symbol): TypedVarTrait = TypVar(sym)

			lazy val typ = NextUniv[Typ[U]] 
    }
    
	case class FormalTyp[U <: WithFormal[U] with AbsObj]() extends Typ[U]{
			lazy val typ = NextUniv[U]
		} 
    
    trait Typ[+U <: WithFormal[U] with AbsObj] extends AbsObj with WithFormal[Typ[U]]{
      val freeVars: Set[Variable] = Set.empty
      
      def formalElem : Typ[U] = FormalTyp[U]()

      val typ: Typ[AbsObj]   
      
      val self = this
     
      
      def -->[V <: WithFormal[V] with AbsObj](that: Typ[V]) = FuncTyp(this, that)		 
                       
			def mkelem(ob: Obj) : U = formalElem[U]      


      trait Obj extends AbsObj with Base[U]{
        def elem : U = mkelem(this)
        
        lazy val typ = self
        
        val obj = this
        
        val freeVars: Set[Variable] = self.freeVars
        
        val id = Identity(this, this)
        
        case object refl extends id.Obj
        
        def =:=(that: Obj) = Identity(this, that)
        
        def =:=(that:AbsObj) = eql(typ)(this, that)

		 
      }
      
      case class TypedVar(sym: Symbol) extends TypedVarTrait


			trait TypedVarTrait extends Obj with Variable{
        override val freeVars: Set[Variable] = Set(this)
        
        def --> (result: AbsObj) = {val tp = result.typ; tp.lambda(this)(result as tp)}
      }

      implicit def ::(sym: Symbol): TypedVarTrait = TypedVar(sym)
      
      def :::(obj: AbsObj) = obj.as(this)
      
      def :::(tryobj: Try[AbsObj])=Try(tryobj.get as this)
      
			def ::(obj: AbsObj) = obj.as(this).elem

      case class Identity(left: Obj, right: Obj) extends ElemTyp{
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
    
    def eql(typ: Typ[AbsObj])(left: AbsObj, right: AbsObj) = typ.Identity(left as typ, right as typ) 
    
        
    case class IndexTyp(tp: Typ[AbsObj]){
      case class ForSection[U <: WithFormal[U] with AbsObj](section: tp.Obj => Typ[U]) extends ElemTyp{
        case class Index(arg: tp.Obj){
          val codomain = section(arg)
          case class Dappl(f: DepFnTyp.Obj) extends codomain.Obj{
            override val freeVars = f.freeVars ++ arg.freeVars
          }
          
          case class DepPair(value: codomain.Obj) extends DepPairTyp.Obj{
            
          }
        }
        
        case object DepFnTyp extends ElemTyp{
	
        	trait FunctionalObj extends Obj{
        		def act(arg: tp.Obj) = {
        			val domain = Index(arg)
        			(domain.Dappl)(this)
        		}
          
        		def apply(arg:tp.Obj) = act(arg)
          
        		def apply(arg: AbsObj) = act(arg.as(tp))
        	}
        	
        }
        
        case object DepPairTyp extends ElemTyp{
          def ::(indx: tp.Obj, obj: AbsObj) = {
            val indxTyp = Index(indx)
            indxTyp.DepPair(obj as indxTyp.codomain)
          }
        
        }
      }            
    }
    
    implicit def indexTyp(tp: Typ[AbsObj]) = IndexTyp(tp) 
    
    def Pi[U<: WithFormal[U] with AbsObj](base: Typ[AbsObj])(section:base.Obj => Typ[U]) ={
    	val indx = IndexTyp(base)
    	val sect = (x: indx.tp.Obj) => section(x as base)
    	indx.ForSection(sect).DepFnTyp
      }
    
    def Sigma[U<: WithFormal[U] with AbsObj](base: Typ[AbsObj])(section:base.Obj => Typ[U]) ={
    	val indx = IndexTyp(base)
    	val sect = (x: indx.tp.Obj) => section(x as base)
    	indx.ForSection(sect).DepPairTyp
      }
 
    
    
    case class FuncTyp[U <: WithFormal[U] with AbsObj](dom: Typ[AbsObj], codom: Typ[U]) extends ElemTyp{     
      
      override val freeVars = dom.freeVars ++ codom.freeVars
      
      case class Appl(f: Obj, arg: dom.Obj) extends codom.Obj{
        override val freeVars = arg.freeVars ++ f.freeVars
        
      }
      
      trait FormalFunction extends FunctionalObj{
        def act(arg: dom.Obj) : codom.Obj = Appl(this, arg)
      }
      

      trait FunctionalObj extends Obj{
        def act(arg: dom.Obj) : codom.Obj  
        
        def apply(arg: dom.Obj): codom.Obj  =  act(arg)
        
        def apply(arg: AbsObj): codom.Obj = act(arg.as(dom))
        
        def apply(tryarg: Try[AbsObj]) = Try(act(tryarg.get as dom))
        
       
      }
       
      case class Defn(defn: dom.Obj => codom.Obj) extends FunctionalObj{ 
          def act(arg: dom.Obj) = defn(arg)
      }
       
    }
     
    val idtest = ('n :: Nat) --> ('n :: Nat)
    
    
    case object ZeroTyp extends ElemTyp
    
    case object OneTyp extends ElemTyp
    
    case object star extends OneTyp.Obj
    
    trait ConstructorDomain{
      val dom: Typ[AbsObj] => Typ[AbsObj] 
      
      def apply(that: Typ[AbsObj]) = dom(that)
    }
    
    
    case class ConstContrDom(typ: Typ[AbsObj]) extends ConstructorDomain{
      val dom = (that: Typ[AbsObj]) => typ
    }
    
    case class ToThis(domdom: ConstructorDomain) extends ConstructorDomain{
      val dom = (that: Typ[AbsObj]) =>  domdom.dom(that) --> that
    }
    
    case class InductiveTyp(constructors: Map[Symbol, ConstructorDomain]) extends ElemTyp{
      val constrs = constructors map (_._2)
      
      def rec(that: Typ[AbsObj]) = {}
    }
    

    
    case object Nat extends ElemTyp{
      case class Rec[U<: WithFormal[U] with AbsObj](that: Typ[U]){
        val fnTyp = FuncTyp(Nat, that)
        type domTyp = fnTyp.dom.Obj
        val tgt = Nat --> (that --> that)
        case class rec(base: that.Obj, step: tgt.Obj) extends fnTyp.FormalFunction{
          val atZero = this(zero.as(fnTyp.dom)).as(that)
          val baseIdtyp = that.Identity(atZero, base)
          case object baseId extends baseIdtyp.Obj
        }
        
      }
      
      def rec[U <: WithFormal[U] with AbsObj](base: AbsObj)(step: FuncTyp[U]) = step.typ match {
        case FuncTyp(Nat, FuncTyp(that, other)) if that == other =>
        val R = Rec(that)
        val stp = step.asInstanceOf[R.tgt.Obj]
        R.rec(base as R.that, stp)
      }
    }
    
    case object zero extends Nat.Obj 
    
    case class succ(n: Nat.Obj) extends Nat.Obj
    
    val one = succ(zero)
 
    def mk(a: Typ[AbsObj], b: Typ[AbsObj]): a.Obj => b.Obj = {assert(a==b); (x:a.Obj) => x.asInstanceOf[b.Obj]}
    
  
}
