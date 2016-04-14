package worksheets
import provingground.HoTT._
import provingground.InductiveTypes._

object HoTTExperiment {
  println("Welcome to the Scala worksheet")
  
  val A = "A" ::Type
  val a = "a" :: A
  lambda(a)(a)
  
  RichTerm(a) :-> a
  
  a :-> a
   
  A :-> A
  
  TermOps(a) :-> a
  def IdFn(A : Typ[Term]) = {
  	val a = "a" :: A
  	lambda(a)(a)
  }

	val Id = {
		val A = "A" :: Type
		lambda(A)({
			val a = "a" :: A
			lambda(a)(a)}
		) }
		
  Id(A)
  
  
  Id(A)(a)
   
  Id.typ
	val MPall = {
		val A = "A" :: Type
		val B = "B" :: Type
		lambda(A)(
			lambda(B)({
			val a = "a" :: A
			val ab = "a->b" :: (A ->: B)
			lambda(a)(
				lambda(ab)(
					ab(a)
					))
			}))
		}
  MPall.typ
	val MP = {
		val A = "A" :: Type
		val B = "B" :: Type
			val a = "a" :: A
			val ab = "a->b" :: (A ->: B)
			lambda(a)(
				lambda(ab)(
					ab(a)
					)
					)
					}
	MP.typ

	val X = "X" :: Type
	val Y = "Y" :: Type
    
    
    
  MP.subs(A, X)
 
 
 A.subs(A, X)
 
 val C = "C" :: Type
 
 (A ->: C).subs(A, X)
 
 (A ->: C).subs(C, X)
  C.subs(A, X)
  
  
  
  
  
  val ac = "a->c" :: (A ->: C)
  ac.subs(C, X)
  val c = "c" :: C
  
  ac(a).subs(a, c)
  val split = applptnterm.unapply(ac(a))
  val argopt = split map (_._2)
  
  
  argopt map (_.subs(a, c))
  
  a.subs(a, c)
  
	val x = "x" :: X
	val xy = "x->y" :: (X ->: Y)
	x.typ
	xy.typ
	xy(x).typ
	
	MPall
	
	MPall.typ
	
	Type.subs(A, X)
	
	val lm = MPall.asInstanceOf[Lambda[Term, Term]]
	
	val v = lm.value
	
	val lv = v.asInstanceOf[Lambda[Term, Term]]
	
	lv.variable
	
	lv.variable.typ
	
	lv.variable.subs(A, X)
	
	
	 
	

	
	MPall(X)
	
	MPall(X).typ
	
	MPall(X)(Y)
	
	MPall(X)(Y).typ
	 
	
	MPall.typ
	
	A ~>: (A ->: A)
  (A ~>: (A ->: A)).subs(A, C)

 

 val W : ConstructorPattern[Term] = IdW
 
  object TestTyp extends SmallTyp{
    override def toString ="implicitType"}
  
  /*
  implicit val aType : Typ[Term] = TestTyp
 
 val cn = "hello" ::: W
 cn.pattern(aType)
 cn.cons
 */

/*
  object BoolType extends InductiveTyp with SmallTyp{
  		   
        lazy val constructors = List(this.constructor(this, "true"), cnstr(this))
  }
  BoolType.constructors
 
 
 object NatTypLong extends InductiveTyp with SmallTyp{
  //                     implicit val self = this
 
           lazy val constructors =List(cnstr(this), cnstr(this -->: this))
 }
 
 val List(zero, succ) = NatTypLong.constructors
 succ.cons.typ */
}