package src.main.scala
import provingground.HoTT._
 

object HoTTExperiment {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val A = "A" ::__                                //> A  : provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT
                                                  //| .Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A
  val a = "a" :: A                                //> a  : provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT
                                                  //| .Term] = (a : A)
  lambda(a)(a)                                    //> res0: provingground.HoTT.FuncTerm[provingground.HoTT.Term with provingground
                                                  //| .HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Term]] = ((a : A)⟼(a : A)
  TermOps(a) :-> a                                //> res1: provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term with provingground.HoTT.Subs[provingground.HoTT.Term]] = ((a : A)⟼(a
                                                  //|  : A)
  def IdFn(A : Typ[Term]) = {
  	val a = "a" :: A
  	lambda(a)(a)
  }                                               //> IdFn: (A: provingground.HoTT.Typ[provingground.HoTT.Term])provingground.HoTT
                                                  //| .FuncTerm[provingground.HoTT.Term with provingground.HoTT.Subs[provingground
                                                  //| .HoTT.Term],provingground.HoTT.Term with provingground.HoTT.Subs[provingGrou
                                                  //| nd.HoTT.Term]]

	val Id = {
		val A = "A" :: __
		lambda(A)({
			val a = "a" :: A
			lambda(a)(a)}
		) }                               //> Id  : provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.HoTT.
                                                  //| Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT
                                                  //| .Term]],provingground.HoTT.FuncTerm[provingground.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provingGr
                                                  //| ound.HoTT.Subs[provingground.HoTT.Term]]] = (A⟼((a : A)⟼(a : A)
  
  Id(A)                                           //> res2: provingground.HoTT.FuncTerm[provingground.HoTT.Term with provingground
                                                  //| .HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Term]] = ((a : A)⟼(a : A)
  
  
  Id(A)(a)                                        //> res3: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoT
                                                  //| T.Term] = (a : A)
   
  Id.typ                                          //> res4: provingground.HoTT.Typ[provingground.HoTT.Term] = Pi((A⟼(A⟶A))
	val MPall = {
		val A = "A" :: __
		val B = "B" :: __
		lambda(A)(
			lambda(B)({
			val a = "a" :: A
			val ab = "a->b" :: (A ->: B)
			lambda(a)(
				lambda(ab)(
					ab(a)
					))
			}))
		}                                 //> MPall  : provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.Ho
                                                  //| TT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.H
                                                  //| oTT.Term]],provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.
                                                  //| HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground
                                                  //| .HoTT.Term]],provingground.HoTT.FuncTerm[provingground.HoTT.Term with provin
                                                  //| gGround.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.FuncTerm[provi
                                                  //| ngGround.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT.Term] with
                                                  //|  provingground.HoTT.Subs[provingground.HoTT.FuncTerm[provingground.HoTT.Term
                                                  //| ,provingground.HoTT.Term]],provingground.HoTT.Term]]]] = (A⟼(B⟼((a : A)�592 ��((a->b : (A⟶B))⟼((a->b : (A⟶B))((a : A)) : B)
  MPall.typ                                       //> res5: provingground.HoTT.Typ[provingground.HoTT.Term] = Pi((A⟼Pi((B⟼(A�
                                                  //| �((A⟶B)⟶B))))
	val MP = {
		val A = "A" :: __
		val B = "B" :: __
			val a = "a" :: A
			val ab = "a->b" :: (A ->: B)
			lambda(a)(
				lambda(ab)(
					ab(a)
					)
					)
					}         //> MP  : provingground.HoTT.FuncTerm[provingground.HoTT.Term with provingground
                                                  //| .HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT.Term] with provin
                                                  //| gGround.HoTT.Subs[provingground.HoTT.FuncTerm[provingground.HoTT.Term,provin
                                                  //| gGround.HoTT.Term]],provingground.HoTT.Term]] = ((a : A)⟼((a->b : (A⟶B))
                                                  //| ⟼((a->b : (A⟶B))((a : A)) : B)
	MP.typ                                    //> res6: provingground.HoTT.Typ[provingground.HoTT.Term] = (A⟶((A⟶B)⟶B))

	val X = "X" :: __                         //> X  : provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT
                                                  //| .Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = X
	val Y = "Y" :: __                         //> Y  : provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT
                                                  //| .Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Y
    
    
    
  MP.subs(A, X)                                   //> res7: provingground.HoTT.FuncTerm[provingground.HoTT.Term with provingground
                                                  //| .HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT.Term] with provin
                                                  //| gGround.HoTT.Subs[provingground.HoTT.FuncTerm[provingground.HoTT.Term,provin
                                                  //| gGround.HoTT.Term]],provingground.HoTT.Term]] = ((a : X)⟼((a->b : (X⟶B))
                                                  //| ⟼((a->b : (X⟶B))((a : X)) : B)
 
 
 A.subs(A, X)                                     //> res8: provingground.HoTT.Typ[provingground.HoTT.Term] = X
 
 val C = "C" :: __                                //> C  : provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT
                                                  //| .Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = C
 
 (A ->: C).subs(A, X)                             //> res9: provingground.HoTT.FuncTyp[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = (X⟶C)
 
 (A ->: C).subs(C, X)                             //> res10: provingground.HoTT.FuncTyp[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term] = (A⟶X)
  C.subs(A, X)                                    //> res11: provingground.HoTT.Typ[provingground.HoTT.Term] = C
  
  
  
  
  
  val ac = "a->c" :: (A ->: C)                    //> ac  : provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term] with provingground.HoTT.Subs[provingground.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.Term,provingground.HoTT.Term]] = (a->c : (A⟶C))
  ac.subs(C, X)                                   //> res12: provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term] = (a->c : (A⟶X))
  val c = "c" :: C                                //> c  : provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoT
                                                  //| T.Term] = (c : C)
  
  ac(a).subs(a, c)                                //> res13: provingground.HoTT.Term = ((a->c : (A⟶C))((c : C)) : C)
  val split = applptnterm.unapply(ac(a))          //> split  : Option[(provingground.HoTT.FuncTerm[provingground.HoTT.Term,provin
                                                  //| gGround.HoTT.Term], provingground.HoTT.Term)] = Some(((a->c : (A⟶C)),(a :
                                                  //|  A)))
  val argopt = split map (_._2)                   //> argopt  : Option[provingground.HoTT.Term] = Some((a : A))
  
  
  argopt map (_.subs(a, c))                       //> res14: Option[provingground.HoTT.Term] = Some((c : C))
  
  a.subs(a, c)                                    //> res15: provingground.HoTT.Term = (c : C)
  
	val x = "x" :: X                          //> x  : provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoT
                                                  //| T.Term] = (x : X)
	val xy = "x->y" :: (X ->: Y)              //> xy  : provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] with provingground.HoTT.Subs[provingground.HoTT.FuncTerm[provingGro
                                                  //| und.HoTT.Term,provingground.HoTT.Term]] = (x->y : (X⟶Y))
	x.typ                                     //> res16: provingground.HoTT.Typ[provingground.HoTT.Term] = X
	xy.typ                                    //> res17: provingground.HoTT.Typ[provingground.HoTT.Term] = (X⟶Y)
	xy(x).typ                                 //> res18: provingground.HoTT.Typ[provingground.HoTT.Term] = Y
	
	MPall                                     //> res19: provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.HoT
                                                  //| T.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.H
                                                  //| oTT.Term]],provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground
                                                  //| .HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingGrou
                                                  //| nd.HoTT.Term]],provingground.HoTT.FuncTerm[provingground.HoTT.Term with pro
                                                  //| vingGround.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.FuncTerm[p
                                                  //| rovingGround.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT.Term]
                                                  //|  with provingground.HoTT.Subs[provingground.HoTT.FuncTerm[provingground.HoT
                                                  //| T.Term,provingground.HoTT.Term]],provingground.HoTT.Term]]]] = (A⟼(B⟼((
                                                  //| a : A)⟼((a->b : (A⟶B))⟼((a->b : (A⟶B))((a : A)) : B)
	
	MPall.typ                                 //> res20: provingground.HoTT.Typ[provingground.HoTT.Term] = Pi((A⟼Pi((B⟼(A
                                                  //| ⟶((A⟶B)⟶B))))
	
	__.subs(A, X)                             //> res21: provingground.HoTT.Universe = _
	
	val lm = MPall.asInstanceOf[Lambda[Term, Term]]
                                                  //> lm  : provingground.HoTT.Lambda[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = (A⟼(B⟼((a : A)⟼((a->b : (A⟶B))⟼((a->b : (A⟶B))((a : A))
                                                  //|  : B)
	
	val v = lm.value                          //> v  : provingground.HoTT.Term = (B⟼((a : A)⟼((a->b : (A⟶B))⟼((a->b :
                                                  //|  (A⟶B))((a : A)) : B)
	
	val lv = v.asInstanceOf[Lambda[Term, Term]]
                                                  //> lv  : provingground.HoTT.Lambda[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = (B⟼((a : A)⟼((a->b : (A⟶B))⟼((a->b : (A⟶B))((a : A)) : B)
                                                  //| 
	
	lv.variable                               //> res22: provingground.HoTT.Term = B
	
	lv.variable.typ                           //> res23: provingground.HoTT.Typ[provingground.HoTT.Term] = _
	
	lv.variable.subs(A, X)                    //> res24: provingground.HoTT.Term = B
	
	val funny = lv.subs(A, X)                 //> funny  : provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term] = (B⟼((a : X)⟼((a->b : (X⟶B))⟼((a->b : (X⟶B))((a : X))
                                                  //|  : B)
	
	val bizarre = funny.asInstanceOf[Lambda[Term, Term]]
                                                  //> bizarre  : provingground.HoTT.Lambda[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term] = (B⟼((a : X)⟼((a->b : (X⟶B))⟼((a->b : (X⟶B))((a : X))
                                                  //|  : B)
	
	bizarre.variable                          //> res25: provingground.HoTT.Term = B
	
	val fa= bizarre.value.asInstanceOf[Lambda[Term, Term]].value.asInstanceOf[Lambda[Term, Term]].value
                                                  //> fa  : provingground.HoTT.Term = ((a->b : (X⟶B))((a : X)) : B)
	applptnterm.unapply(fa)                   //> res26: Option[(provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingG
                                                  //| round.HoTT.Term], provingground.HoTT.Term)] = Some(((a->b : (X⟶B)),(a : X
                                                  //| )))
	
	val inner = applptnterm.unapply(fa).get._2//> inner  : provingground.HoTT.Term = (a : X)
	
	inner.subs(A, X)                          //> res27: provingground.HoTT.Term = (a : X)
	
	inner.typ == A                            //> res28: Boolean = false
	
	inner.asInstanceOf[Symbolic].name         //> res29: provingground.HoTT.AnySym = a
	 
	
	v.subs(A, X)                              //> res30: provingground.HoTT.Term = (B⟼((a : X)⟼((a->b : (X⟶B))⟼((a->b
                                                  //|  : (X⟶B))((a : X)) : B)
	
	MPall(X)                                  //> res31: provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.HoT
                                                  //| T.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.H
                                                  //| oTT.Term]],provingground.HoTT.FuncTerm[provingground.HoTT.Term with proving
                                                  //| Ground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.FuncTerm[provi
                                                  //| ngGround.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT.Term] wit
                                                  //| h provingground.HoTT.Subs[provingground.HoTT.FuncTerm[provingground.HoTT.Te
                                                  //| rm,provingground.HoTT.Term]],provingground.HoTT.Term]]] = (B⟼((a : X)⟼(
                                                  //| (a->b : (X⟶B))⟼((a->b : (X⟶B))((a : X)) : B)
	
	MPall(X).typ                              //> res32: provingground.HoTT.Typ[provingground.HoTT.Term] = Pi((B⟼(X⟶((X�
                                                  //| �B)⟶B)))
	
	MPall(X)(Y)                               //> res33: provingground.HoTT.FuncTerm[provingground.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.FuncTerm[provingGr
                                                  //| ound.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT.Term] with pr
                                                  //| ovingGround.HoTT.Subs[provingground.HoTT.FuncTerm[provingground.HoTT.Term,p
                                                  //| rovingGround.HoTT.Term]],provingground.HoTT.Term]] = ((a : X)⟼((a->b : (X
                                                  //| ⟶Y))⟼((a->b : (X⟶Y))((a : X)) : Y)
	
	MPall(X)(Y).typ                           //> res34: provingground.HoTT.Typ[provingground.HoTT.Term] = (X⟶((X⟶Y)⟶Y)
                                                  //| )
	 
	
	MPall.typ                                 //> res35: provingground.HoTT.Typ[provingground.HoTT.Term] = Pi((A⟼Pi((B⟼(A
                                                  //| ⟶((A⟶B)⟶B))))
	
	A ~>: (A ->: A)                           //> res36: provingground.HoTT.PiTyp[provingground.HoTT.Term,provingground.HoTT.
                                                  //| FuncTerm[provingground.HoTT.Term,provingground.HoTT.Term]] = Pi((A⟼(A⟶A
                                                  //| ))
  (A ~>: (A ->: A)).subs(A, C)                    //> res37: provingground.HoTT.PiTyp[provingground.HoTT.Term,provingground.HoTT.
                                                  //| FuncTerm[provingground.HoTT.Term,provingground.HoTT.Term]] = Pi((C⟼(C⟶C
                                                  //| ))

 

 val W : PolyPtn[Term] = IdW                      //> W  : provingground.HoTT.PolyPtn[provingground.HoTT.Term] = IdW
 
  object TestTyp extends SmallTyp{
    override def toString ="implicitType"}
  
  /*
  implicit val aType : Typ[Term] = TestTyp
 
 val cn = "hello" ::: W
 cn.pattern(aType)
 cn.cons
 */
 
  object BoolType extends InductiveTyp with SmallTyp{
  		   
        lazy val constructors = List(this.constructor(this, "true"), cnstr(this))
  }
  BoolType.constructors                           //> res38: List[provingground.HoTT.Constructor] = List(ConstructorDefn(IdW,(tru
                                                  //| e : src.main.scala.HoTTExperiment$$anonfun$main$1$BoolType$2$@3906173b)), C
                                                  //| onstructorDefn(IdW,(provingground.HoTT$Typ$newname$2$@3edf3fbc : src.main.s
                                                  //| cala.HoTTExperiment$$anonfun$main$1$BoolType$2$@3906173b)))
 
 
 object NatTyp extends InductiveTyp with SmallTyp{
  //                     implicit val self = this
 
           lazy val constructors =List(cnstr(this), cnstr(this -->: this))
 }
 
 val List(zero, succ) = NatTyp.constructors       //> zero  : provingground.HoTT.Constructor = ConstructorDefn(IdW,(provingground
                                                  //| .HoTT$Typ$newname$2$@3129373c : src.main.scala.HoTTExperiment$$anonfun$main
                                                  //| $1$NatTyp$2$@8e95d5b))
                                                  //| succ  : provingground.HoTT.Constructor = ConstructorDefn(CnstFncPtn(src.mai
                                                  //| n.scala.HoTTExperiment$$anonfun$main$1$NatTyp$2$@8e95d5b,IdW),(provingGroun
                                                  //| d.HoTT$Typ$newname$2$@10f005ef : (src.main.scala.HoTTExperiment$$anonfun$ma
                                                  //| in$1$NatTyp$2$@8e95d5b⟶src.main.scala.HoTTExperiment$$anonfun$main$1$NatT
                                                  //| yp$2$@8e95d5b)))
 succ.cons.typ                                    //> res39: provingground.HoTT.Typ[provingground.HoTT.Term] = (src.main.scala.Ho
                                                  //| TTExperiment$$anonfun$main$1$NatTyp$2$@8e95d5b⟶src.main.scala.HoTTExperim
                                                  //| ent$$anonfun$main$1$NatTyp$2$@8e95d5b)
}