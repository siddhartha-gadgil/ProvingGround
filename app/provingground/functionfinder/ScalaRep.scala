package provingground.functionfinder
import provingground.HoTT._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object ScalaRep {
	/**
	 * Representation by a scala object of a HoTT term 
	 * 
	 * @typparam U the HoTT type represented
	 * @typparam V scala type representing the given object. 
	 */
  trait ScalaRep[+U <: Term, V]{
    /**
     * HoTT type represented.
     */
    val typ : Typ[U]
    
    /**
     * scala type of representing object.
     */
    type tpe = V
    
    /**
     * HoTT object from the scala one.
     */
    def apply(v: V) : U
    
    /**
     * pattern for matching application of the scala object.
     */
    def unapply(u: Term) : Option[V]
    
    /**
     * scalarep for function
     */
    def -->:[W <: Term : TypeTag, X, UU >: U <: Term : TypeTag](that : ScalaRep[W, X]) = 
      FuncRep[W, X, UU, V](that, this)
     
    /**
     *   scalarep for sum type.
     */  
    def ++[UU >: U <: Term with Subs[UU]: TypeTag, X <: Term with Subs[X]: TypeTag, Y](
        codrepfmly: V => ScalaRep[X, Y]) = SigmaRep[UU, V, X, Y](this, codrepfmly)
  }
  
  /**
   * constant  term.
   */
  	trait ConstTerm[T] extends Term{
	  val value: T
	  
	  type scalaType = T
	  
      def subs(x : Term, y: Term) = if (x==this) y else this 
    }
  
  /**
   * formal extension of a function.
   */
  def extend[T, U <: Term : TypeTag](fn: T => U, functerm: FuncTerm[Term, U], codom: Typ[U]): Term => U = {
	  case c: ConstTerm[T] => fn(c.value)
	  case arg: Term => codom.symbObj(ApplnSym(functerm, arg))
	}
  
  /**
   * scala object wrapped to give a HoTT constant with type declared.
   */
  case class SimpleConst[V](value: V, typ: Typ[Term]) extends ConstTerm[V]{
    override def toString = value.toString
  }
  
  /**
   * Representation by  wrapping.
   */
  case class SimpleRep[V](typ: Typ[Term]) extends ScalaRep[Term, V]{
    def apply(v: V) = SimpleConst(v, typ)
    
    def unapply(u : Term) : Option[V] = u match  {
      case smp: SimpleConst[V] if smp.typ == typ => Some(smp.value)
      case _ => None
    }
  }
 
  /**
   * A term representing itself.
   */
  implicit class IdRep[U <: Term : TypeTag](val typ: Typ[U]) extends ScalaRep[U, U]{
    def apply(v : U) = v
    
    def unapply(u: Term): Option[U] = u match{
      case t: Term => Some(t.asInstanceOf[U])
      case _ => None
    }
  }
  

  /**
   * Representations for functions given ones for the domain and codomain.
   */
  case class FuncRep[U <: Term : TypeTag, V, X <: Term : TypeTag, Y](
      domrep: ScalaRep[U, V], codomrep: ScalaRep[X, Y]) extends ScalaRep[FuncObj[U, X], V => Y]{
    val typ = domrep.typ ->: codomrep.typ
    
    def apply(f: V => Y) : FuncObj[U, X] = ExtendedFunction(f, domrep, codomrep)
    
    def unapply(u: Term) : Option[V => Y] = u match {
      case ext: ExtendedFunction[_, V, _, Y] if ext.domrep == domrep && ext.codomrep == codomrep => Some(ext.dfn)
      case _ => None
    }
  }
  
  /**
   * Formal extension of a function given by a definition and representations for 
   * domain and codomain.
   */
  case class ExtendedFunction[U <: Term : TypeTag, V, X <: Term : TypeTag, Y](dfn: V => Y, 
      domrep: ScalaRep[U, V], codomrep: ScalaRep[X, Y]) extends FuncObj[U, X]{
    
	  val dom = domrep.typ
	  
	  val codom = codomrep.typ
	  
	  val typ = dom ->: codom
	  
	  def apply(u : U) = u match {
	    case domrep(v) => codomrep(dfn(v))
	    case _ => codom.symbObj(ApplnSym(this, u))
	  }
	  
	  val domobjtpe: reflect.runtime.universe.Type = typeOf[U]
	  
	  val codomobjtpe: reflect.runtime.universe.Type = typeOf[X]
	  
	  
	  def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = (x, y) match {
	    case (u, v: FuncObj[U ,X]) if u == this => v
	    case _ => this
	  }
    
  }
  
  
  /**
   * Function rep with codomain representing itself. Should perhaps use  IdRep instead.
   */
  case class SimpleFuncRep[U <: Term : TypeTag, V, X <: Term : TypeTag](
      domrep: ScalaRep[U, V], codom: Typ[X]) extends ScalaRep[FuncTerm[U, X], V => X]{
    val typ = domrep.typ ->: codom
    
    def apply(f: V => X) = SimpleExtendedFunction(f, domrep, codom)
    
    def unapply(u: Term) : Option[V => X] = u match {
      case ext: SimpleExtendedFunction[_, V, X] if ext.domrep == domrep && ext.codom == codom => Some(ext.dfn)
      case _ => None
    }
  }
  
  /**
   * Extended function with codomain a type. Perhaps use IdRep.
   */
  case class SimpleExtendedFunction[U <: Term : TypeTag, V, X <: Term : TypeTag](dfn: V => X, 
      domrep: ScalaRep[U, V], codom: Typ[X]) extends FuncObj[U, X]{
    
	  val dom = domrep.typ
	  
	  
	  val typ = dom ->: codom
	  
	  def apply(u : U) = u match {
	    case domrep(v) => dfn(v)
	    case _ => codom.symbObj(ApplnSym(this, u))
	  }
	  
	  val domobjtpe: reflect.runtime.universe.Type = typeOf[U]
	  
	  val codomobjtpe: reflect.runtime.universe.Type = typeOf[X]
	  
	  
	  def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = (x, y) match {
	    case (u, v: FuncObj[U ,X]) if u == this => v
	    case _ => this
	  }
    
  }
  
  
  
  
  case class SigmaRep[U <: Term with Subs[U] : TypeTag, V, X <: Term with Subs[X]: TypeTag, Y](domrep: ScalaRep[U, V],
      codrepfmly: V => ScalaRep[X, Y]) extends ScalaRep[Term, (V, Y)]{

    
//    val rep = SimpleFuncRep(domrep, __)
    
    val rep = FuncRep(domrep, __)
    
    val fibers = rep((v: V) => codrepfmly(v).typ)
    
    lazy val typ = SigmaTyp(fibers)
    
    def apply(vy: (V, Y))  = DepPair(domrep(vy._1), codrepfmly(vy._1)(vy._2), fibers)
    
    def unapply(u: Term) = u match {
      case DepPair(domrep(v), vy, _) =>
        val codrep = codrepfmly(v)
        vy match {
          case codrep(y) => Some((v, y))
          case _ => None
        }
      case _ => None
    }
  }
  
  /**
   * Formal extendsion of a dependent function given scalareps for the domain and codomains.
   */
  case class DepFuncRep[U <: Term : TypeTag, V, X <: Term : TypeTag, Y](
      domrep: ScalaRep[U, V], codomreps: V => ScalaRep[X, Y], fibers: TypFamily[U, X]) extends ScalaRep[FuncTerm[U, X], V => Y]{
    val typ = PiTyp(fibers)
    
    def apply(f: V => Y) : FuncTerm[U, X] = ExtendedDepFunction(f, domrep, codomreps, fibers)
    
    def unapply(u: Term) : Option[V => Y] = u match {
      case ext: ExtendedDepFunction[_, V, _, Y] if ext.domrep == domrep && ext.codomreps == codomreps => Some(ext.dfn)
      case _ => None
    }
  }
  
  /**
   * implicit class associated to a type family to create dependent functions scalareps.
   */
  implicit class FmlyReps[U <: Term: TypeTag, X <: Term : TypeTag](fibers: TypFamily[U, X]){
        
    def ~>:[V](domrep : ScalaRep[U, V]) = {
      DepFuncRep(domrep, (v: V) => IdRep(fibers(domrep(v))), fibers)
    }
  }
  
    /**
   * implicit class associated to a family of scalareps to create dependent functions scalareps.
   */
  implicit class RepSection[U <: Term : TypeTag, X <: Term : TypeTag, Y](section: U => ScalaRep[X, Y]){
    
    def ~>:[V](domrep : ScalaRep[U, V]) = {
      val fmly = (u: U) => section(u).typ
      val fibers = typFamily(domrep.typ, fmly)
        
      DepFuncRep(domrep, (v: V) => section(domrep(v)), fibers)
    }
  }
  
  /**
   * formal extension of a dependent function.
   */
    case class ExtendedDepFunction[U <: Term : TypeTag, V, X <: Term : TypeTag, Y](dfn: V => Y, 
      domrep: ScalaRep[U, V], codomreps: V => ScalaRep[X, Y], fibers: TypFamily[U, X]) extends FuncTerm[U, X]{
    
	  val dom = domrep.typ
	  
	  val depcodom : U => Typ[X] = (arg : U) => fibers(arg)
	  
	  val typ = PiTyp(fibers)
	  
	  def apply(u : U) = u match {
	    case domrep(v) => codomreps(v)(dfn(v))
	    case arg => fibers(arg).symbObj(ApplnSym(this, arg))
	  }
	  
	  val domobjtpe: reflect.runtime.universe.Type = typeOf[Term]
	  
	  val codomobjtpe: reflect.runtime.universe.Type = typeOf[U]
	  

	  
	  def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = (x, y) match {
	    case (u, v: FuncTerm[U ,X]) if u == this => v
	    case _ => this
	  }
    
  }
  
    /*
    case class InclRep[U <: Term : TypeTag, V <: U : TypeTag](typ: Typ[U]) extends ScalaRep[U, V]{
      def apply(v: V) = v
      
      def unapply(u: Term) = u match {
        case v : V => Some(v)
        case _ => None
      }
    }
    */
    
    object dsl{
      def i[V](typ: Typ[Term]) = SimpleRep[V](typ)
      
      def s[U <: Term with Subs[U] : TypeTag, V, X <: Term with Subs[X]: TypeTag, Y](domrep: ScalaRep[U, V])(
      codrepfmly: V => ScalaRep[X, Y]) = SigmaRep(domrep, codrepfmly)
    }
    
   
}