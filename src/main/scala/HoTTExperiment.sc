package main.scala
import provingGround.HoTT._


object HoTTExperiment {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val A = "A" ::__                                //> A  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = (A : _)
  val a = "a" :: A                                //> a  : provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoTT
                                                  //| .Term] = (a : (A : _))
  lambda(a)(a)                                    //> res0: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGround
                                                  //| .HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingGround.HoTT.Term]] = ((a : (A : _))) |-> (a : (A : _))
  TermOps(a) :-> a                                //> res1: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT
                                                  //| .Term with provingGround.HoTT.Subs[provingGround.HoTT.Term]] = ((a : (A : _)
                                                  //| )) |-> (a : (A : _))
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
                                                  //| ound.HoTT.Subs[provingGround.HoTT.Term]]] = ((A : _)) |-> ((a : (A : _))) |-
                                                  //| > (a : (A : _))
  
  Id(A)                                           //> res2: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGround
                                                  //| .HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingGround.HoTT.Term]] = ((a : (A : _))) |-> (a : (A : _))
  
  
  Id(A)(a)                                        //> res3: provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoT
                                                  //| T.Term] = (a : (A : _))
   
  Id.typ                                          //> res4: provingGround.HoTT.Typ[provingGround.HoTT.Term] = PiTyp(((A : _)) |-> 
                                                  //| ((A : _) -> (A : _)))
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
                                                  //| ,provingGround.HoTT.Term]],provingGround.HoTT.Term]]]] = ((A : _)) |-> ((B :
                                                  //|  _)) |-> ((a : (A : _))) |-> ((a->b : ((A : _) -> (B : _)))) |-> ((a->b : ((
                                                  //| A : _) -> (B : _)))((a : (A : _))) : (B : _))
  MPall.typ                                       //> res5: provingGround.HoTT.Typ[provingGround.HoTT.Term] = PiTyp(((A : _)) |-> 
                                                  //| PiTyp(((B : _)) |-> ((A : _) -> (((A : _) -> (B : _)) -> (B : _)))))
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
                                                  //| gGround.HoTT.Term]],provingGround.HoTT.Term]] = ((a : (A : _))) |-> ((a->b :
                                                  //|  ((A : _) -> (B : _)))) |-> ((a->b : ((A : _) -> (B : _)))((a : (A : _))) : 
                                                  //| (B : _))
	MP.typ                                    //> res6: provingGround.HoTT.Typ[provingGround.HoTT.Term] = ((A : _) -> (((A : _
                                                  //| ) -> (B : _)) -> (B : _)))

	val X = "X" :: __                         //> X  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = (X : _)
	val Y = "Y" :: __                         //> Y  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = (Y : _)
    
    
    
  MP.subs(A, X)                                   //> res7: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGround
                                                  //| .HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] with provin
                                                  //| gGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provin
                                                  //| gGround.HoTT.Term]],provingGround.HoTT.Term]] = ((a : (X : _))) |-> ((a->b :
                                                  //|  ((X : _) -> (B : _)))) |-> ((a->b : ((X : _) -> (B : _)))((a : (X : _))) : 
                                                  //| (B : _))
 
 
 A.subs(A, X)                                     //> res8: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (X : _)
 
 val C = "C" :: __                                //> C  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = (C : _)
 
 (A ->: C).subs(A, X)                             //> res9: provingGround.HoTT.FuncTyp[provingGround.HoTT.Term,provingGround.HoTT.
                                                  //| Term] = ((X : _) -> (C : _))
 
 (A ->: C).subs(C, X)                             //> res10: provingGround.HoTT.FuncTyp[provingGround.HoTT.Term,provingGround.HoTT
                                                  //| .Term] = ((A : _) -> (X : _))
  C.subs(A, X)                                    //> res11: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (C : _)
  
  
  
  
  
  val ac = "a->c" :: (A ->: C)                    //> ac  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT
                                                  //| .Term] with provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.Term,provingGround.HoTT.Term]] = (a->c : ((A : _) -> (C : _)))
  ac.subs(C, X)                                   //> res12: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.Ho
                                                  //| TT.Term] = (a->c : ((A : _) -> (X : _)))
  val c = "c" :: C                                //> c  : provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoT
                                                  //| T.Term] = (c : (C : _))
  
  ac(a).subs(a, c)                                //> res13: provingGround.HoTT.Term = ((a->c : ((A : _) -> (C : _)))((c : (C : _
                                                  //| ))) : (C : _))
  val split = applptnterm.unapply(ac(a))          //> split  : Option[(provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provin
                                                  //| gGround.HoTT.Term], provingGround.HoTT.Term)] = Some(((a->c : ((A : _) -> (
                                                  //| C : _))),(a : (A : _))))
  val argopt = split map (_._2)                   //> argopt  : Option[provingGround.HoTT.Term] = Some((a : (A : _)))
  
  
  argopt map (_.subs(a, c))                       //> res14: Option[provingGround.HoTT.Term] = Some((c : (C : _)))
  
  a.subs(a, c)                                    //> res15: provingGround.HoTT.Term = (c : (C : _))
  
	val x = "x" :: X                          //> x  : provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoT
                                                  //| T.Term] = (x : (X : _))
	val xy = "x->y" :: (X ->: Y)              //> xy  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoT
                                                  //| T.Term] with provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGro
                                                  //| und.HoTT.Term,provingGround.HoTT.Term]] = (x->y : ((X : _) -> (Y : _)))
	x.typ                                     //> res16: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (X : _)
	xy.typ                                    //> res17: provingGround.HoTT.Typ[provingGround.HoTT.Term] = ((X : _) -> (Y : _
                                                  //| ))
	xy(x).typ                                 //> res18: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (Y : _)
	
	MPall                                     //> res19: provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround.HoT
                                                  //| T.Term] with provingGround.HoTT.Subs[provingGround.HoTT.Typ[provingGround.H
                                                  //| oTT.Term]],provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround
                                                  //| .HoTT.Term] with provingGround.HoTT.Subs[provingGround.HoTT.Typ[provingGrou
                                                  //| nd.HoTT.Term]],provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with pro
                                                  //| vingGround.HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[p
                                                  //| rovingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term]
                                                  //|  with provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoT
                                                  //| T.Term,provingGround.HoTT.Term]],provingGround.HoTT.Term]]]] = ((A : _)) |-
                                                  //| > ((B : _)) |-> ((a : (A : _))) |-> ((a->b : ((A : _) -> (B : _)))) |-> ((a
                                                  //| ->b : ((A : _) -> (B : _)))((a : (A : _))) : (B : _))
	
	MPall.typ                                 //> res20: provingGround.HoTT.Typ[provingGround.HoTT.Term] = PiTyp(((A : _)) |-
                                                  //| > PiTyp(((B : _)) |-> ((A : _) -> (((A : _) -> (B : _)) -> (B : _)))))
	
	__.subs(A, X)                             //> res21: provingGround.HoTT.Universe = _
	
	val lm = MPall.asInstanceOf[Lambda[Term, Term]]
                                                  //> lm  : provingGround.HoTT.Lambda[provingGround.HoTT.Term,provingGround.HoTT.
                                                  //| Term] = ((A : _)) |-> ((B : _)) |-> ((a : (A : _))) |-> ((a->b : ((A : _) -
                                                  //| > (B : _)))) |-> ((a->b : ((A : _) -> (B : _)))((a : (A : _))) : (B : _))
	
	val v = lm.value                          //> v  : provingGround.HoTT.Term = ((B : _)) |-> ((a : (A : _))) |-> ((a->b : (
                                                  //| (A : _) -> (B : _)))) |-> ((a->b : ((A : _) -> (B : _)))((a : (A : _))) : (
                                                  //| B : _))
	
	val lv = v.asInstanceOf[Lambda[Term, Term]]
                                                  //> lv  : provingGround.HoTT.Lambda[provingGround.HoTT.Term,provingGround.HoTT.
                                                  //| Term] = ((B : _)) |-> ((a : (A : _))) |-> ((a->b : ((A : _) -> (B : _)))) |
                                                  //| -> ((a->b : ((A : _) -> (B : _)))((a : (A : _))) : (B : _))
	
	lv.variable                               //> res22: provingGround.HoTT.Term = (B : _)
	
	lv.variable.typ                           //> res23: provingGround.HoTT.Typ[provingGround.HoTT.Term] = _
	
	lv.variable.subs(A, X)                    //> res24: provingGround.HoTT.Term = (B : _)
	
	val funny = lv.subs(A, X)                 //> funny  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.
                                                  //| HoTT.Term] = ((B : _)) |-> ((a : (X : _))) |-> ((a->b : ((X : _) -> (B : _)
                                                  //| ))) |-> ((a->b : ((X : _) -> (B : _)))((a : (X : _))) : (B : _))
	
	val bizarre = funny.asInstanceOf[Lambda[Term, Term]]
                                                  //> bizarre  : provingGround.HoTT.Lambda[provingGround.HoTT.Term,provingGround.
                                                  //| HoTT.Term] = ((B : _)) |-> ((a : (X : _))) |-> ((a->b : ((X : _) -> (B : _)
                                                  //| ))) |-> ((a->b : ((X : _) -> (B : _)))((a : (X : _))) : (B : _))
	
	bizarre.variable                          //> res25: provingGround.HoTT.Term = (B : _)
	
	val fa= bizarre.value.asInstanceOf[Lambda[Term, Term]].value.asInstanceOf[Lambda[Term, Term]].value
                                                  //> fa  : provingGround.HoTT.Term = ((a->b : ((X : _) -> (B : _)))((a : (X : _)
                                                  //| )) : (B : _))
	applptnterm.unapply(fa)                   //> res26: Option[(provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingG
                                                  //| round.HoTT.Term], provingGround.HoTT.Term)] = Some(((a->b : ((X : _) -> (B 
                                                  //| : _))),(a : (X : _))))
	
	val inner = applptnterm.unapply(fa).get._2//> inner  : provingGround.HoTT.Term = (a : (X : _))
	
	inner.subs(A, X)                          //> res27: provingGround.HoTT.Term = (a : (X : _))
	
	inner.typ == A                            //> res28: Boolean = false
	
	inner.asInstanceOf[Symbolic[Any]].name    //> res29: Any = a
	 
	
	v.subs(A, X)                              //> res30: provingGround.HoTT.Term = ((B : _)) |-> ((a : (X : _))) |-> ((a->b :
                                                  //|  ((X : _) -> (B : _)))) |-> ((a->b : ((X : _) -> (B : _)))((a : (X : _))) :
                                                  //|  (B : _))
	
	MPall(X)                                  //> res31: provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround.HoT
                                                  //| T.Term] with provingGround.HoTT.Subs[provingGround.HoTT.Typ[provingGround.H
                                                  //| oTT.Term]],provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with proving
                                                  //| Ground.HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provi
                                                  //| ngGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] wit
                                                  //| h provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Te
                                                  //| rm,provingGround.HoTT.Term]],provingGround.HoTT.Term]]] = ((B : _)) |-> ((a
                                                  //|  : (X : _))) |-> ((a->b : ((X : _) -> (B : _)))) |-> ((a->b : ((X : _) -> (
                                                  //| B : _)))((a : (X : _))) : (B : _))
	
	MPall(X).typ                              //> res32: provingGround.HoTT.Typ[provingGround.HoTT.Term] = PiTyp(((B : _)) |-
                                                  //| > ((X : _) -> (((X : _) -> (B : _)) -> (B : _))))
	
	MPall(X)(Y)                               //> res33: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provingGr
                                                  //| ound.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] with pr
                                                  //| ovingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,p
                                                  //| rovingGround.HoTT.Term]],provingGround.HoTT.Term]] = ((a : (X : _))) |-> ((
                                                  //| a->b : ((X : _) -> (Y : _)))) |-> ((a->b : ((X : _) -> (Y : _)))((a : (X : 
                                                  //| _))) : (Y : _))
	
	MPall(X)(Y).typ                           //> res34: provingGround.HoTT.Typ[provingGround.HoTT.Term] = ((X : _) -> (((X :
                                                  //|  _) -> (Y : _)) -> (Y : _)))
	 
	
	MPall.typ                                 //> res35: provingGround.HoTT.Typ[provingGround.HoTT.Term] = PiTyp(((A : _)) |-
                                                  //| > PiTyp(((B : _)) |-> ((A : _) -> (((A : _) -> (B : _)) -> (B : _)))))
	
}