package src.main.scala
import provingGround.HoTT._
 

object HoTTExperiment {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val A = "A" ::__                                //> A  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = A
  val a = "a" :: A                                //> a  : provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoTT
                                                  //| .Term] = (a : A)
  lambda(a)(a)                                    //> res0: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGround
                                                  //| .HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingGround.HoTT.Term]] = ((a : A)⟼(a : A)
  TermOps(a) :-> a                                //> res1: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT
                                                  //| .Term with provingGround.HoTT.Subs[provingGround.HoTT.Term]] = ((a : A)⟼(a
                                                  //|  : A)
  def IdFn(A : Typ[Term]) = {
  	val a = "a" :: A
  	lambda(a)(a)
  }                                               //> IdFn: (A: provingGround.HoTT.Typ[provingGround.HoTT.Term])provingGround.HoTT
                                                  //| .FuncTerm[provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround
                                                  //| .HoTT.Term],provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGrou
                                                  //| nd.HoTT.Term]]

	val Id = {
		val A = "A" :: __
		lambda(A)({
			val a = "a" :: A
			lambda(a)(a)}
		) }                               //> Id  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround.HoTT.
                                                  //| Term] with provingGround.HoTT.Subs[provingGround.HoTT.Typ[provingGround.HoTT
                                                  //| .Term]],provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.Term with provingGr
                                                  //| ound.HoTT.Subs[provingGround.HoTT.Term]]] = (A⟼((a : A)⟼(a : A)
  
  Id(A)                                           //> res2: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGround
                                                  //| .HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingGround.HoTT.Term]] = ((a : A)⟼(a : A)
  
  
  Id(A)(a)                                        //> res3: provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoT
                                                  //| T.Term] = (a : A)
   
  Id.typ                                          //> res4: provingGround.HoTT.Typ[provingGround.HoTT.Term] = Pi((A⟼(A⟶A))
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
		}                                 //> MPall  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround.Ho
                                                  //| TT.Term] with provingGround.HoTT.Subs[provingGround.HoTT.Typ[provingGround.H
                                                  //| oTT.Term]],provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround.
                                                  //| HoTT.Term] with provingGround.HoTT.Subs[provingGround.HoTT.Typ[provingGround
                                                  //| .HoTT.Term]],provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provin
                                                  //| gGround.HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provi
                                                  //| ngGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] with
                                                  //|  provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Term
                                                  //| ,provingGround.HoTT.Term]],provingGround.HoTT.Term]]]] = (A⟼(B⟼((a : A)�592 ��((a->b : (A⟶B))⟼((a->b : (A⟶B))((a : A)) : B)
  MPall.typ                                       //> res5: provingGround.HoTT.Typ[provingGround.HoTT.Term] = Pi((A⟼Pi((B⟼(A�
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
					}         //> MP  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGround
                                                  //| .HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] with provin
                                                  //| gGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provin
                                                  //| gGround.HoTT.Term]],provingGround.HoTT.Term]] = ((a : A)⟼((a->b : (A⟶B))
                                                  //| ⟼((a->b : (A⟶B))((a : A)) : B)
	MP.typ                                    //> res6: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (A⟶((A⟶B)⟶B))

	val X = "X" :: __                         //> X  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = X
	val Y = "Y" :: __                         //> Y  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = Y
    
    
    
  MP.subs(A, X)                                   //> res7: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGround
                                                  //| .HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] with provin
                                                  //| gGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provin
                                                  //| gGround.HoTT.Term]],provingGround.HoTT.Term]] = ((a : X)⟼((a->b : (X⟶B))
                                                  //| ⟼((a->b : (X⟶B))((a : X)) : B)
 
 
 A.subs(A, X)                                     //> res8: provingGround.HoTT.Typ[provingGround.HoTT.Term] = X
 
 val C = "C" :: __                                //> C  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = C
 
 (A ->: C).subs(A, X)                             //> res9: provingGround.HoTT.FuncTyp[provingGround.HoTT.Term,provingGround.HoTT.
                                                  //| Term] = (X⟶C)
 
 (A ->: C).subs(C, X)                             //> res10: provingGround.HoTT.FuncTyp[provingGround.HoTT.Term,provingGround.HoTT
                                                  //| .Term] = (A⟶X)
  C.subs(A, X)                                    //> res11: provingGround.HoTT.Typ[provingGround.HoTT.Term] = C
  
  
  
  
  
  val ac = "a->c" :: (A ->: C)                    //> ac  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT
                                                  //| .Term] with provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.Term,provingGround.HoTT.Term]] = (a->c : (A⟶C))
  ac.subs(C, X)                                   //> res12: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.Ho
                                                  //| TT.Term] = (a->c : (A⟶X))
  val c = "c" :: C                                //> c  : provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoT
                                                  //| T.Term] = (c : C)
  
  ac(a).subs(a, c)                                //> res13: provingGround.HoTT.Term = ((a->c : (A⟶C))((c : C)) : C)
  val split = applptnterm.unapply(ac(a))          //> split  : Option[(provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provin
                                                  //| gGround.HoTT.Term], provingGround.HoTT.Term)] = Some(((a->c : (A⟶C)),(a :
                                                  //|  A)))
  val argopt = split map (_._2)                   //> argopt  : Option[provingGround.HoTT.Term] = Some((a : A))
  
  
  argopt map (_.subs(a, c))                       //> res14: Option[provingGround.HoTT.Term] = Some((c : C))
  
  a.subs(a, c)                                    //> res15: provingGround.HoTT.Term = (c : C)
  
	val x = "x" :: X                          //> x  : provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoT
                                                  //| T.Term] = (x : X)
	val xy = "x->y" :: (X ->: Y)              //> xy  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoT
                                                  //| T.Term] with provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGro
                                                  //| und.HoTT.Term,provingGround.HoTT.Term]] = (x->y : (X⟶Y))
	x.typ                                     //> res16: provingGround.HoTT.Typ[provingGround.HoTT.Term] = X
	xy.typ                                    //> res17: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (X⟶Y)
	xy(x).typ                                 //> res18: provingGround.HoTT.Typ[provingGround.HoTT.Term] = Y
	
	MPall                                     //> res19: provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround.HoT
                                                  //| T.Term] with provingGround.HoTT.Subs[provingGround.HoTT.Typ[provingGround.H
                                                  //| oTT.Term]],provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround
                                                  //| .HoTT.Term] with provingGround.HoTT.Subs[provingGround.HoTT.Typ[provingGrou
                                                  //| nd.HoTT.Term]],provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with pro
                                                  //| vingGround.HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[p
                                                  //| rovingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term]
                                                  //|  with provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoT
                                                  //| T.Term,provingGround.HoTT.Term]],provingGround.HoTT.Term]]]] = (A⟼(B⟼((
                                                  //| a : A)⟼((a->b : (A⟶B))⟼((a->b : (A⟶B))((a : A)) : B)
	
	MPall.typ                                 //> res20: provingGround.HoTT.Typ[provingGround.HoTT.Term] = Pi((A⟼Pi((B⟼(A
                                                  //| ⟶((A⟶B)⟶B))))
	
	__.subs(A, X)                             //> res21: provingGround.HoTT.Universe = _
	
	val lm = MPall.asInstanceOf[Lambda[Term, Term]]
                                                  //> lm  : provingGround.HoTT.Lambda[provingGround.HoTT.Term,provingGround.HoTT.
                                                  //| Term] = (A⟼(B⟼((a : A)⟼((a->b : (A⟶B))⟼((a->b : (A⟶B))((a : A))
                                                  //|  : B)
	
	val v = lm.value                          //> v  : provingGround.HoTT.Term = (B⟼((a : A)⟼((a->b : (A⟶B))⟼((a->b :
                                                  //|  (A⟶B))((a : A)) : B)
	
	val lv = v.asInstanceOf[Lambda[Term, Term]]
                                                  //> lv  : provingGround.HoTT.Lambda[provingGround.HoTT.Term,provingGround.HoTT.
                                                  //| Term] = (B⟼((a : A)⟼((a->b : (A⟶B))⟼((a->b : (A⟶B))((a : A)) : B)
                                                  //| 
	
	lv.variable                               //> res22: provingGround.HoTT.Term = B
	
	lv.variable.typ                           //> res23: provingGround.HoTT.Typ[provingGround.HoTT.Term] = _
	
	lv.variable.subs(A, X)                    //> res24: provingGround.HoTT.Term = B
	
	val funny = lv.subs(A, X)                 //> funny  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.
                                                  //| HoTT.Term] = (B⟼((a : X)⟼((a->b : (X⟶B))⟼((a->b : (X⟶B))((a : X))
                                                  //|  : B)
	
	val bizarre = funny.asInstanceOf[Lambda[Term, Term]]
                                                  //> bizarre  : provingGround.HoTT.Lambda[provingGround.HoTT.Term,provingGround.
                                                  //| HoTT.Term] = (B⟼((a : X)⟼((a->b : (X⟶B))⟼((a->b : (X⟶B))((a : X))
                                                  //|  : B)
	
	bizarre.variable                          //> res25: provingGround.HoTT.Term = B
	
	val fa= bizarre.value.asInstanceOf[Lambda[Term, Term]].value.asInstanceOf[Lambda[Term, Term]].value
                                                  //> fa  : provingGround.HoTT.Term = ((a->b : (X⟶B))((a : X)) : B)
	applptnterm.unapply(fa)                   //> res26: Option[(provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingG
                                                  //| round.HoTT.Term], provingGround.HoTT.Term)] = Some(((a->b : (X⟶B)),(a : X
                                                  //| )))
	
	val inner = applptnterm.unapply(fa).get._2//> inner  : provingGround.HoTT.Term = (a : X)
	
	inner.subs(A, X)                          //> res27: provingGround.HoTT.Term = (a : X)
	
	inner.typ == A                            //> res28: Boolean = false
	
	inner.asInstanceOf[Symbolic].name         //> res29: provingGround.HoTT.AnySym = a
	 
	
	v.subs(A, X)                              //> res30: provingGround.HoTT.Term = (B⟼((a : X)⟼((a->b : (X⟶B))⟼((a->b
                                                  //|  : (X⟶B))((a : X)) : B)
	
	MPall(X)                                  //> res31: provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround.HoT
                                                  //| T.Term] with provingGround.HoTT.Subs[provingGround.HoTT.Typ[provingGround.H
                                                  //| oTT.Term]],provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with proving
                                                  //| Ground.HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provi
                                                  //| ngGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] wit
                                                  //| h provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Te
                                                  //| rm,provingGround.HoTT.Term]],provingGround.HoTT.Term]]] = (B⟼((a : X)⟼(
                                                  //| (a->b : (X⟶B))⟼((a->b : (X⟶B))((a : X)) : B)
	
	MPall(X).typ                              //> res32: provingGround.HoTT.Typ[provingGround.HoTT.Term] = Pi((B⟼(X⟶((X�
                                                  //| �B)⟶B)))
	
	MPall(X)(Y)                               //> res33: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provingGr
                                                  //| ound.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] with pr
                                                  //| ovingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,p
                                                  //| rovingGround.HoTT.Term]],provingGround.HoTT.Term]] = ((a : X)⟼((a->b : (X
                                                  //| ⟶Y))⟼((a->b : (X⟶Y))((a : X)) : Y)
	
	MPall(X)(Y).typ                           //> res34: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (X⟶((X⟶Y)⟶Y)
                                                  //| )
	 
	
	MPall.typ                                 //> res35: provingGround.HoTT.Typ[provingGround.HoTT.Term] = Pi((A⟼Pi((B⟼(A
                                                  //| ⟶((A⟶B)⟶B))))
	
	A ~>: (A ->: A)                           //> res36: provingGround.HoTT.PiTyp[provingGround.HoTT.Term,provingGround.HoTT.
                                                  //| FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term]] = Pi((A⟼(A⟶A
                                                  //| ))
  (A ~>: (A ->: A)).subs(A, C)                    //> res37: provingGround.HoTT.PiTyp[provingGround.HoTT.Term,provingGround.HoTT.
                                                  //| FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term]] = Pi((C⟼(C⟶C
                                                  //| ))

 

 val W : PolyPtn[Term] = IdW                      //> W  : provingGround.HoTT.PolyPtn[provingGround.HoTT.Term] = IdW
 
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
  BoolType.constructors                           //> res38: List[provingGround.HoTT.Constructor] = List(ConstructorDefn(IdW,(tru
                                                  //| e : src.main.scala.HoTTExperiment$$anonfun$main$1$BoolType$2$@3906173b)), C
                                                  //| onstructorDefn(IdW,(provingGround.HoTT$Typ$newname$2$@3edf3fbc : src.main.s
                                                  //| cala.HoTTExperiment$$anonfun$main$1$BoolType$2$@3906173b)))
 
 
 object NatTyp extends InductiveTyp with SmallTyp{
  //                     implicit val self = this
 
           lazy val constructors =List(cnstr(this), cnstr(this -->: this))
 }
 
 val List(zero, succ) = NatTyp.constructors       //> zero  : provingGround.HoTT.Constructor = ConstructorDefn(IdW,(provingGround
                                                  //| .HoTT$Typ$newname$2$@3129373c : src.main.scala.HoTTExperiment$$anonfun$main
                                                  //| $1$NatTyp$2$@8e95d5b))
                                                  //| succ  : provingGround.HoTT.Constructor = ConstructorDefn(CnstFncPtn(src.mai
                                                  //| n.scala.HoTTExperiment$$anonfun$main$1$NatTyp$2$@8e95d5b,IdW),(provingGroun
                                                  //| d.HoTT$Typ$newname$2$@10f005ef : (src.main.scala.HoTTExperiment$$anonfun$ma
                                                  //| in$1$NatTyp$2$@8e95d5b⟶src.main.scala.HoTTExperiment$$anonfun$main$1$NatT
                                                  //| yp$2$@8e95d5b)))
 succ.cons.typ                                    //> res39: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (src.main.scala.Ho
                                                  //| TTExperiment$$anonfun$main$1$NatTyp$2$@8e95d5b⟶src.main.scala.HoTTExperim
                                                  //| ent$$anonfun$main$1$NatTyp$2$@8e95d5b)
}