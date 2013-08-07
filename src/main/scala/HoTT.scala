package provingGround

import scala.language.implicitConversions 
import scala.util._

object HoTT{
  
   
    trait AbsObj {
      val typ: Typ[AbsObj]
      
      val freeVars: Set[Variable]
      
      def as[U <: AbsObj](that: Typ[U]) = {assert (typ==that); this.asInstanceOf[that.Obj]}

			def as(that: FormalTyp) = {assert (typ==that); this.asInstanceOf[that.FormalObj]}
     }
         

    trait Elem[+A]{def elem: A}
    
    implicit def baseElem[A](b: Elem[A]): A = b.elem
    
    trait Variable
    
	
 	   
		implicit class AsFormalTyp[U<: AbsObj](typ: Typ[U]) extends FormalTyp{
			implicit class AsFormalObj(obj: Obj) extends FormalObj
			} 


		 

    trait FormalTyp extends Typ[AbsObj]{
    	
    	lazy val typ: Typ[AbsObj] = FirstUniv

			def -->(that: FormalTyp) = FuncTyp(this, that)

			case class TypedVar(sym: Symbol) extends FormalObj with Variable

			implicit def asObj(sym: Symbol) : FormalObj = TypedVar(sym)

			def ::(sym: Symbol) = TypedVar(sym)

			trait FormalObj extends Obj{
				def elem : AbsObj = this
				}

			case class lambda(x: Variable)(result: Obj) extends FormalObj{
        override val freeVars = result.freeVars - x
      }

    }


		trait Univ[U<: AbsObj] extends Typ[Typ[U]]{
			trait TypObj extends Obj with Typ[U]{
				def elem: Typ[U] = this

				override val freeVars: Set[Variable] = Set.empty
				}

			case class TypedVar(sym: Symbol) extends TypObj with Variable

			def ::(sym: Symbol) =TypedVar(sym)

			implicit class InUniv(tp: Typ[U]) extends TypObj
			} 
     
    
    object FirstUniv extends Typ[FormalTyp] with Univ[AbsObj]{
      lazy val typ = NextUniv[Typ[AbsObj], Typ[Typ[AbsObj]]]			
    }
    
		case class NextUniv[U <: AbsObj, V <: Typ[U]]() extends Typ[V] with Univ[U]{
			lazy val typ = NextUniv[V, Typ[V]] 
    }
    

    
    trait Typ[+U <: AbsObj] extends AbsObj {
      val freeVars: Set[Variable] = Set.empty
           
      val typ: Typ[AbsObj]   
      
      val self = this


      trait Obj extends AbsObj with Elem[U]{
        def elem : U 
        
        lazy val typ = self
        
        val obj = this
        
        val freeVars: Set[Variable] = self.freeVars
        
        val id = Identity(this, this)
        
        case object refl extends id.FormalObj
        
        def =:=(that: Obj) = Identity(this, that)
        
        def =:=(that:AbsObj) = eql(typ)(this, that)		 
      }
      
    


			trait TypedVarTrait extends Obj with Variable{
        override val freeVars: Set[Variable] = Set(this)
        
     //   def --> (result: AbsObj) = {val tp = result.typ; tp.lambda(this)(result as tp)}
      }
      
      def :::(obj: AbsObj) = obj.as(this)
      
      def :::(tryobj: Try[AbsObj])=Try(tryobj.get as this)
      
			def ::(obj: AbsObj) = obj.as(this).elem

      case class Identity(left: Obj, right: Obj) extends FormalTyp{
        override val freeVars: Set[Variable] = self.freeVars
      }
      
    //  case class lambda(x: Variable)(result: Obj) extends Obj{
    //    override val freeVars = result.freeVars - x
    //  }      
      
    }
    
    implicit def me(arg: AbsObj): arg.typ.Obj = arg as arg.typ
    
    val i = Nat.lambda('x :: Nat)('x)
    
  //  def lambda(x: Variable)(res: AbsObj) = {
  //    val tp = res.typ
  //    tp.lambda(x)(res as tp)
  //  }
    
    def eql(typ: Typ[AbsObj])(left: AbsObj, right: AbsObj) = typ.Identity(left as typ, right as typ) 
    
        
    case class IndexTyp(tp: Typ[AbsObj]){
      case class ForSection(section: tp.Obj => FormalTyp) extends FormalTyp{
        case class Index(arg: tp.Obj){
          val codomain = section(arg)
          case class Dappl(f: DepFnTyp.Obj) extends codomain.FormalObj{
            override val freeVars = f.freeVars ++ arg.freeVars
          }
          
          case class DepPair(value: codomain.Obj) extends DepPairTyp.FormalObj{
            
          }
        }
        
        case object DepFnTyp extends FormalTyp{
	
        	trait FunctionalObj extends FormalObj{
        		def act(arg: tp.Obj) = {
        			val domain = Index(arg)
        			(domain.Dappl)(this)
        		}
          
        		def apply(arg:tp.Obj) = act(arg)
          
        		def apply(arg: AbsObj) = act(arg.as(tp))
        	}
        	
        }
        
        case object DepPairTyp extends FormalTyp{
          def ::(indx: tp.Obj, obj: AbsObj) = {
            val indxTyp = Index(indx)
            indxTyp.DepPair(obj as indxTyp.codomain)
          }
        
        }
      }            
    }
    
    implicit def indexTyp(tp: Typ[AbsObj]) = IndexTyp(tp) 
    
    def Pi(base: Typ[AbsObj])(section:base.Obj => FormalTyp) ={
    	val indx = IndexTyp(base)
    	val sect = (x: indx.tp.Obj) => section(x as base)
    	indx.ForSection(sect).DepFnTyp
      }
    
    def Sigma(base: Typ[AbsObj])(section:base.Obj => FormalTyp) ={
    	val indx = IndexTyp(base)
    	val sect = (x: indx.tp.Obj) => section(x as base)
    	indx.ForSection(sect).DepPairTyp
      }
 
    
		case class FuncTyp[U<: AbsObj](dom: Typ[U], codom: FormalTyp) extends FormalTyp{
	    case class Appl(f: Obj, arg: dom.Obj) extends codom.FormalObj{
  	    override val freeVars = arg.freeVars ++ f.freeVars
        }     
      
      override val freeVars = dom.freeVars ++ codom.freeVars
     
      
      trait FormalFunction extends FunctionalObj{
        def act(arg: dom.Obj) : codom.Obj = Appl(this, arg)
      }
      

      trait FunctionalObj extends FormalObj{
        def act(arg: dom.Obj) : codom.Obj  
        
        def apply(arg: dom.Obj): codom.Obj  =  act(arg)
        
        def apply(arg: AbsObj): codom.Obj = act(arg.as(dom))
        
        def apply(tryarg: Try[AbsObj]) = Try(act(tryarg.get as dom))
        
       
      }
       
      case class Defn(defn: dom.Obj => codom.Obj) extends FunctionalObj{ 
          def act(arg: dom.Obj) = defn(arg)
      }
       
    }
     
  //  val idtest = ('n :: Nat) --> ('n :: Nat)
    
    
    case object ZeroTyp extends FormalTyp
    
    case object OneTyp extends FormalTyp
    
    case object star extends OneTyp.FormalObj
    
    trait ConstructorDomain{
      val dom: FormalTyp => FormalTyp 
      
      def apply(that: FormalTyp) = dom(that)
    }
    
    
    case class ConstContrDom(typ: FormalTyp) extends ConstructorDomain{
      val dom = (that: FormalTyp) => typ
    }
    
    case class ToThis(domdom: ConstructorDomain) extends ConstructorDomain{
      val dom = (that: FormalTyp) =>  domdom.dom(that) --> that
    }
    
    case class InductiveTyp(constructors: Map[Symbol, ConstructorDomain]) extends FormalTyp{
      val constrs = constructors map (_._2)
      
      def rec(that: FormalTyp) = {}
    }
    

    
    case object Nat extends FormalTyp{
      case class Rec(that: FormalTyp){
        val fnTyp = FuncTyp(Nat, that)
        type domTyp = fnTyp.dom.Obj
        val tgt = Nat --> (that --> that)
        case class rec(base: that.Obj, step: tgt.Obj) extends fnTyp.FormalFunction{
          val atZero = this(zero.as(fnTyp.dom)).as(that)
          val baseIdtyp = that.Identity(atZero, base)
          case object baseId extends baseIdtyp.FormalObj
        }
        
      }
      
      def rec[U <: AbsObj](base: AbsObj)(step: FuncTyp[U]) = step.typ match {
        case FuncTyp(Nat, FuncTyp(that : FormalTyp, other)) if that == other =>
        val R = Rec(that)
        val stp = step.asInstanceOf[R.tgt.Obj]
        R.rec(base as R.that, stp)
      }
    }
    
    case object zero extends Nat.FormalObj 
    
    case class succ(n: Nat.Obj) extends Nat.FormalObj
    
    val one = succ(zero)
 
    def mk(a: Typ[AbsObj], b: Typ[AbsObj]): a.Obj => b.Obj = {assert(a==b); (x:a.Obj) => x.asInstanceOf[b.Obj]}
    
  
}
