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
    
    trait Variable extends AbsObj
    
	
 	   
		implicit class AsFormalTyp[U<: AbsObj](typ: Typ[U]) extends FormalTyp{
			implicit class AsFormalObj(obj: Obj) extends FormalObj
			} 


    trait EffectiveTyp[+U <: AbsObj] extends Typ[U]{
      def idObj[A](id: A): Obj

			def typedVar[A](sym: A): VarObj
    }
		 

    trait FormalTyp extends EffectiveTyp[AbsObj]{
    	
    	lazy val typ: Typ[AbsObj] = FirstUniv

			def -->[V <: AbsObj](that: EffectiveTyp[V]) = FuncTyp(this, that)

			case class TypedVar[A](sym: A) extends FormalObj with VarObj

			def typedVar[A](sym: A) = TypedVar(sym)

			case class IdObj[A](name: A) extends FormalObj
			
			def idObj[A](id: A) = IdObj(id)
			
			implicit def asObj(sym: Symbol) : FormalObj = typedVar(sym)

			def ::(sym: Symbol) = typedVar(sym)

			trait FormalObj extends Obj{
				def elem : AbsObj = this
				
		//		def -->:(x: Variable) = LambdaObj(x, this)

		//		def ->:(x: Variable) = asLambda(x)

		//		def asLambda(x: Variable) = {
		//			val dom = x.typ
		//			val funcTyp = FuncTyp(dom, typ)
		//			x match {
		//				case varx: funcTyp.dom.VarObj => funcTyp.Lambda(varx, this as funcTyp.codom) 
		//				}
		//			}
				}

//			case class lambda(x: Variable)(result: Obj) extends FormalObj{
//					override val freeVars = result.freeVars - x
//			}
			
			case class LambdaObj(x: Variable, result: Obj) extends FormalObj{
			  override val freeVars = result.freeVars - x
			}


    }


//		def lambda(x: Variable)(res: AbsObj) = res.typ match {
//				case ttp: FormalTyp => ttp.lambda(x)(res as ttp)
//				}


		trait Univ[U<: AbsObj] extends EffectiveTyp[Typ[U]]{
			trait TypObj extends Obj with Typ[U]{
				def elem: Typ[U] = this

				override val freeVars: Set[Variable] = Set.empty

		//		def ->:(x: Variable) = asLambda(x)

		//		def asLambda(x: Variable) = {
		//			val dom = x.typ
		//			val funcTyp = TypFamily(dom, typ.asInstanceOf[Univ[U]])
		//			x match {
		//				case varx: funcTyp.dom.VarObj => funcTyp.Lambda(varx, this as funcTyp.codom) 
		//				}
		//			}
				}

			case class TypedVar[A](sym: A) extends TypObj with VarObj

			def typedVar[A](sym: A) = TypedVar(sym)

			case class IdObj[A](name: A) extends TypObj

			def idObj[A](name: A) = IdObj(name)

			def ::(sym: Symbol) = typedVar(sym)

			implicit class InUniv(tp: Typ[U]) extends TypObj
			} 
     
    
    object FirstUniv extends Typ[FormalTyp] with Univ[AbsObj]{
      lazy val typ = NextUniv[Typ[AbsObj], Typ[Typ[AbsObj]]]			
    }
    
		case class NextUniv[U <: AbsObj, V <: Typ[U]]() extends Typ[V] with Univ[U]{
			lazy val typ = NextUniv[V, Typ[V]] 
    }
    

    
    trait Typ[+U <: AbsObj] extends AbsObj {self =>
      val freeVars: Set[Variable] = Set.empty
           
      val typ: Typ[AbsObj]   
   

      trait VarObj extends Obj with Variable

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
      
   
      
      def :::(obj: AbsObj) = obj.as(this)
      
      def :::(tryobj: Try[AbsObj])=Try(tryobj.get as this)
      
			def ::(obj: AbsObj) = obj.as(this).elem

      case class Identity(left: Obj, right: Obj) extends FormalTyp{
        override val freeVars: Set[Variable] = self.freeVars
      }
            
      
    }

	case class Formal[T](implicit tag: scala.reflect.runtime.universe.TypeTag[T]) extends FormalTyp{
		implicit class AsFormalObj(ob: T) extends FormalObj
		
		def ::(t: T) = AsFormalObj(t)
		} 

	
    implicit def me(arg: AbsObj): arg.typ.Obj = arg as arg.typ
    
 //   val i = Nat.lambda('x :: Nat)('x)
    
    
    def eql(typ: Typ[AbsObj])(left: AbsObj, right: AbsObj) = typ.Identity(left as typ, right as typ) 
    
    3 :: Formal[Long]
    
	
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
 
    
	case class FuncTyp[U<: AbsObj, V <: AbsObj](dom: Typ[U], codom: EffectiveTyp[V]) extends FormalTyp{
	  //  case class Appl(f: Obj, arg: dom.Obj) extends codom.FormalObj{
  	//    override val freeVars = arg.freeVars ++ f.freeVars
    //    }     
      
      override val freeVars = dom.freeVars ++ codom.freeVars
     
      
      trait FormalFunction extends FunctionalObj{
        def act(arg: dom.Obj) : codom.Obj = codom.idObj((this, arg))
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
      
      case class Lambda(variable: dom.VarObj, value: codom.Obj) extends FormalObj
       
    }
     

		def lambda[U<: AbsObj, V <: AbsObj](dom: Typ[U], codom: EffectiveTyp[V])(variable: dom.VarObj, value: codom.Obj) ={
					val fnTyp = FuncTyp(dom, codom)
					fnTyp.Lambda(variable.asInstanceOf[fnTyp.dom.VarObj], value as fnTyp.codom)
				}

 @deprecated("use FuncTyp", "27/08/13")	case class TypFamily[U<: AbsObj, V<: AbsObj](dom: Typ[U], codom: Univ[V]) extends FormalTyp{
	    case class Appl(f: Obj, arg: dom.Obj) extends codom.TypObj{
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
       
      case class Lambda(variable: dom.VarObj, value: codom.Obj) extends FormalObj
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
      
      def rec[U <: AbsObj, V<: AbsObj](base: AbsObj)(step: FuncTyp[U, V]) = step.typ match {
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
