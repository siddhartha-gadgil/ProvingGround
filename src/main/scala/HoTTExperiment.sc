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
   
  Id.typ                                          //> res2: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (_ -> ((A : _) -> (A
                                                  //|  : _)))
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
  MPall.typ                                       //> res3: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (_ -> (_ -> ((A : _)
                                                  //|  -> (((A : _) -> (B : _)) -> (B : _)))))
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
	MP.typ                                    //> res4: provingGround.HoTT.Typ[provingGround.HoTT.Term] = ((A : _) -> (((A : _
                                                  //| ) -> (B : _)) -> (B : _)))

	val X = "X" :: __                         //> X  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = (X : _)
	val Y = "Y" :: __                         //> Y  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = (Y : _)
    
    
    
  MP.subs(A, X)                                   //> res5: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGround
                                                  //| .HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] with provin
                                                  //| gGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provin
                                                  //| gGround.HoTT.Term]],provingGround.HoTT.Term]] = ((a : ((X : _) -> (((X : _) 
                                                  //| -> (B : _)) -> (B : _))))) |-> ((a->b : (((X : _) -> (B : _)) -> (B : _)))) 
                                                  //| |-> ((a->b : ((X : _) -> (B : _)))((a : (A : _))) : (B : _))
 
 
 A.subs(A, X)                                     //> res6: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (X : _)
 
 val C = "C" :: __                                //> C  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = (C : _)
 
 (A ->: C).subs(A, X)                             //> res7: provingGround.HoTT.FuncTyp[provingGround.HoTT.Term,provingGround.HoTT.
                                                  //| Term] = ((X : _) -> (C : _))
 
 (A ->: C).subs(C, X)                             //> res8: provingGround.HoTT.FuncTyp[provingGround.HoTT.Term,provingGround.HoTT.
                                                  //| Term] = ((A : _) -> (X : _))
  
  val ac = "a->c" :: (A ->: C)                    //> ac  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT
                                                  //| .Term] with provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.Term,provingGround.HoTT.Term]] = (a->c : ((A : _) -> (C : _)))
  ac.subs(C, X)                                   //> res9: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT
                                                  //| .Term] = (a->c : ((A : _) -> (X : _)))
  
			val x = "x" :: X          //> x  : provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoTT
                                                  //| .Term] = (x : (X : _))
			val xy = "x->y" :: (X ->: Y)
                                                  //> xy  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoT
                                                  //| T.Term] with provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGro
                                                  //| und.HoTT.Term,provingGround.HoTT.Term]] = (x->y : ((X : _) -> (Y : _)))
	x.typ                                     //> res10: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (X : _)
	xy.typ                                    //> res11: provingGround.HoTT.Typ[provingGround.HoTT.Term] = ((X : _) -> (Y : _
                                                  //| ))
	xy(x).typ                                 //> res12: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (Y : _)
	
	MPall                                     //> res13: provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround.HoT
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
	
	MPall.typ                                 //> res14: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (_ -> (_ -> ((A : 
                                                  //| _) -> (((A : _) -> (B : _)) -> (B : _)))))
	
	MPall(X)                                  //> res15: provingGround.HoTT.FuncTerm[provingGround.HoTT.Typ[provingGround.HoT
                                                  //| T.Term] with provingGround.HoTT.Subs[provingGround.HoTT.Typ[provingGround.H
                                                  //| oTT.Term]],provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with proving
                                                  //| Ground.HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provi
                                                  //| ngGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] wit
                                                  //| h provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Te
                                                  //| rm,provingGround.HoTT.Term]],provingGround.HoTT.Term]]] = ((B : (_ -> ((X :
                                                  //|  _) -> (((X : _) -> (B : _)) -> (B : _)))))) |-> ((a : ((X : _) -> (((X : _
                                                  //| ) -> (B : _)) -> (B : _))))) |-> ((a->b : (((X : _) -> (B : _)) -> (B : _))
                                                  //| )) |-> ((a->b : ((X : _) -> (B : _)))((a : (A : _))) : (B : _))
	
	MPall(X).typ                              //> res16: provingGround.HoTT.Typ[provingGround.HoTT.Term] = ((_ -> ((X : _) ->
                                                  //|  (((X : _) -> (B : _)) -> (B : _)))) -> (((X : _) -> (((X : _) -> (B : _)) 
                                                  //| -> (B : _))) -> ((((X : _) -> (B : _)) -> (B : _)) -> (B : _))))
	
	MPall(X)(Y)                               //> res17: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.FuncTerm[provingGr
                                                  //| ound.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT.Term] with pr
                                                  //| ovingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,p
                                                  //| rovingGround.HoTT.Term]],provingGround.HoTT.Term]] = ((a : ((X : _) -> (((X
                                                  //|  : _) -> (B : _)) -> (B : _))))) |-> ((a->b : (((X : _) -> (B : _)) -> (B :
                                                  //|  _)))) |-> ((a->b : ((X : _) -> (B : _)))((a : (A : _))) : (B : _))
	
	MPall(X)(Y).typ                           //> res18: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (((X : _) -> (((X 
                                                  //| : _) -> (B : _)) -> (B : _))) -> ((((X : _) -> (B : _)) -> (B : _)) -> (B :
                                                  //|  _)))
	
	
}