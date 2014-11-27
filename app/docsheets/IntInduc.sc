package docsheets

import provingground.HoTT._

import provingground.functionfinder.IntTypes._

import provingground.functionfinder.ScalaRep._

object IntInduc {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val n = N.rep                                   //> n  : provingground.functionfinder.ScalaRep.SimpleRep[Long] = SimpleRep(N)
  
  val three = N.rep(3)                            //> three  : provingground.functionfinder.ScalaRep.SimpleConst[Long] = 3

	val six = N.sum(three)(three)             //> six  : provingground.HoTT.Term = 6
	
	val nine = N.prod(three)(three)           //> nine  : provingground.HoTT.Term = 9
	
	val induc = inducFn[Term] _               //> induc  : (provingground.HoTT.Term, Long => (provingground.HoTT.Term => provi
                                                  //| ngground.HoTT.Term), Long, provingground.HoTT.Term => provingground.HoTT.Ter
                                                  //| m) => provingground.HoTT.Term = <function4>
	
	val fntyp = N ->: N                       //> fntyp  : provingground.HoTT.FuncTyp[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term] = (N->N)
	
	
	N.rep.typ                                 //> res0: provingground.HoTT.Typ[provingground.HoTT.Term] = N
	
	N.rep.typ ->: N.rep.typ                   //> res1: provingground.HoTT.FuncTyp[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = (N->N)
	
	
	val ind = induccurry[Term]                //> ind  : provingground.HoTT.Term => ((Long => (provingground.HoTT.Term => prov
                                                  //| ingground.HoTT.Term)) => (Long => provingground.HoTT.Term)) = <function1>
	
	val rec =recursion(N)                     //> rec  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT
                                                  //| .FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.H
                                                  //| oTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]] = <function1>
                                                  //| 
	
	recN                                      //> res2: provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.HoTT.
                                                  //| Term],provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]]] = <function1>
                                                  //| 
	
	recN(N)                                   //> res3: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]] = <function1>
	
	recN(N).typ                               //> res4: provingground.HoTT.Typ[provingground.HoTT.Term] = (N->((N->(N->N))->(N
                                                  //| ->N)))
	
	val recc = recN(N)                        //> recc  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.H
                                                  //| oTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.
                                                  //| HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]] = <function1
                                                  //| >
	
	val fn = recc(N.rep(0))(N.sum)            //> fn  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = (<function1>(<function1>) : (N->N))
	
	fn(N.rep(0))                              //> res5: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(0) : N)
                                                  //| 
	
	fn(three)                                 //> res6: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(3) : N)
                                                  //| 
	
	fn.typ                                    //> res7: provingground.HoTT.Typ[provingground.HoTT.Term] = (N->N)
	
	fn(three).typ                             //> res8: provingground.HoTT.Typ[provingground.HoTT.Term] = N
	
	N.rep.unapply(fn(three))                  //> res9: Option[Long] = None

	N.rep.unapply(six)                        //> res10: Option[Long] = Some(6)
                    
                    
	
	val add = (k: Long) => (l : Term) => N.sum(N.rep(k))(l)
                                                  //> add  : Long => (provingground.HoTT.Term => provingground.HoTT.Term) = <funct
                                                  //| ion1>
	
	add(3)(three)                             //> res11: provingground.HoTT.Term = 6
	
	
	val sumto5 = inducFn(n(0), add, 5)        //> sumto5  : provingground.HoTT.Term = 15
	
	ind(N.rep(0))(add)(5)                     //> res12: provingground.HoTT.Term = 15
	
	val rrr = (n -->: (N -->: N)) -->: (n -->: N)
                                                  //> rrr  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Func
                                                  //| Obj[provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Te
                                                  //| rm,provingground.HoTT.Term]],Long => (provingground.HoTT.Term => provinggrou
                                                  //| nd.HoTT.Term),provingground.HoTT.FuncObj[provingground.HoTT.Term,provinggrou
                                                  //| nd.HoTT.Term],Long => provingground.HoTT.Term] = FuncRep(FuncRep(SimpleRep(N
                                                  //| ),FuncRep(provingground.functionfinder.ScalaRep$IdRep@f07d752,provingground.
                                                  //| functionfinder.ScalaRep$IdRep@4bc95033)),FuncRep(SimpleRep(N),provingground.
                                                  //| functionfinder.ScalaRep$IdRep@db06b24))
	
	
	ind(N.rep(0))                             //> res13: (Long => (provingground.HoTT.Term => provingground.HoTT.Term)) => (Lo
                                                  //| ng => provingground.HoTT.Term) = <function1>
  
  
  val x = rrr(ind(N.rep(0)))                      //> x  : provingground.HoTT.FuncObj[provingground.HoTT.FuncObj[provingground.HoT
                                                  //| T.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term]] = <function1>
	
	x(N.sum)(three)                           //> res14: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(3) : N
                                                  //| )
	
	val in = ind(N.rep(0))(add)               //> in  : Long => provingground.HoTT.Term = <function1>
	
	val fnrep = (n -->: N)                    //> fnrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.T
                                                  //| erm,Long,provingground.HoTT.Term,provingground.HoTT.Term] = FuncRep(SimpleR
                                                  //| ep(N),provingground.functionfinder.ScalaRep$IdRep@394a0f81)
  fnrep((k: Long) => N.rep(k))                    //> res15: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = <function1>
  
	fnrep(in)(three)                          //> res16: provingground.HoTT.Term = 6

	fnrep(in)(six)                            //> res17: provingground.HoTT.Term = 21
	
	
	// The above is completely successful.
	
	val idrep = n -->: n                      //> idrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.T
                                                  //| erm,Long,provingground.HoTT.Term,Long] = FuncRep(SimpleRep(N),SimpleRep(N))
                                                  //| 
	
	idrep((k: Long) => 2 *k)(three)           //> res18: provingground.HoTT.Term = 6
	
	val addrep = n -->: N -->: N              //> addrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.
                                                  //| Term,Long,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term],provingground.HoTT.Term => provingground.HoTT.Term] = FuncRep(Si
                                                  //| mpleRep(N),FuncRep(provingground.functionfinder.ScalaRep$IdRep@58a13b90,pro
                                                  //| vingground.functionfinder.ScalaRep$IdRep@414d9865))
	
	addrep(add)(three)(three)                 //> res19: provingground.HoTT.Term = 6
	
	val addterm = addrep(add)                 //> addterm  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground
                                                  //| .HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] = <function
                                                  //| 1>
	
	val recrep = N -->: addrep -->: fnrep     //> recrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.
                                                  //| Term,provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.Ho
                                                  //| TT.Term,provingground.HoTT.Term]],provingground.HoTT.FuncObj[provingground.
                                                  //| HoTT.Term,provingground.HoTT.Term]],(Long => (provingground.HoTT.Term => pr
                                                  //| ovingground.HoTT.Term)) => (Long => provingground.HoTT.Term)] = FuncRep(pro
                                                  //| vingground.functionfinder.ScalaRep$IdRep@4d503061,FuncRep(FuncRep(SimpleRep
                                                  //| (N),FuncRep(provingground.functionfinder.ScalaRep$IdRep@58a13b90,provinggro
                                                  //| und.functionfinder.ScalaRep$IdRep@414d9865)),FuncRep(SimpleRep(N),provinggr
                                                  //| ound.functionfinder.ScalaRep$IdRep@394a0f81)))
	
	val readd =  addrep.unapply(addterm).get  //> readd  : Long => (provingground.HoTT.Term => provingground.HoTT.Term) = <fu
                                                  //| nction1>
	readd(3)(six)                             //> res20: provingground.HoTT.Term = 9
	
	recrep(ind)                               //> res21: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provinggroun
                                                  //| d.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]] = <functi
                                                  //| on1>
	
	recrep.unapply(recrep(ind))               //> res22: Option[provingground.HoTT.Term => ((Long => (provingground.HoTT.Term
                                                  //|  => provingground.HoTT.Term)) => (Long => provingground.HoTT.Term))] = Some
                                                  //| (<function1>)
	recrep(ind).typ                           //> res23: provingground.HoTT.Typ[provingground.HoTT.Term] = (N->((N->(N->N))->
                                                  //| (N->N)))

	recrep(ind)(N.rep(0))                     //> res24: provingground.HoTT.FuncObj[provingground.HoTT.FuncObj[provingground.
                                                  //| HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term,provinggroun
                                                  //| d.HoTT.Term]] = <function1>
	
	val toapply = recrep(ind)(N.rep(0))(N.sum)//> toapply  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground
                                                  //| .HoTT.Term] = (<function1>(<function1>) : (N->N))

	toapply.asInstanceOf[FuncSymb[Term, Term]]//> res25: provingground.HoTT.FuncSymb[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term] = (<function1>(<function1>) : (N->N))

	recrep(ind)(N.rep(0))(N.sum)(N.rep(5))    //> res26: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(5) : 
                                                  //| N)

	recrep(ind)(N.rep(0))(N.sum)(N.rep(5)).typ//> res27: provingground.HoTT.Typ[provingground.HoTT.Term] = N

	val NN = IdRep(N)                         //> NN  : provingground.functionfinder.ScalaRep.IdRep[provingground.HoTT.Term] 
                                                  //| = provingground.functionfinder.ScalaRep$IdRep@4b9857ee
	
	NN(three)                                 //> res28: provingground.HoTT.Term = 3
	
	val r = n -->: N                          //> r  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Term,
                                                  //| Long,provingground.HoTT.Term,provingground.HoTT.Term] = FuncRep(SimpleRep(N
                                                  //| ),provingground.functionfinder.ScalaRep$IdRep@7eb4f107)
	
	r((k: Long) => N.rep(k))(three)           //> res29: provingground.HoTT.Term = 3

	val rr = N -->: n -->: N                  //> rr  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Term
                                                  //| ,provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term
                                                  //| ,provingground.HoTT.Term],Long => provingground.HoTT.Term] = FuncRep(provin
                                                  //| gground.functionfinder.ScalaRep$IdRep@43772c93,FuncRep(SimpleRep(N),proving
                                                  //| ground.functionfinder.ScalaRep$IdRep@450f897f))
	
	val rsum = rr((x: Term) => (k : Long) => N.sum(x)(N.rep(k)))
                                                  //> rsum  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] = <function1>

	rsum(three)(three)                        //> res30: provingground.HoTT.Term = 6
	

	recrep(ind)(N.rep(0))(N.sum)              //> res31: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = (<function1>(<function1>) : (N->N))

	val evalrep = (n -->: N) -->: N           //> evalrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT
                                                  //| .FuncObj[provingground.HoTT.Term,provingground.HoTT.Term],Long => provinggr
                                                  //| ound.HoTT.Term,provingground.HoTT.Term,provingground.HoTT.Term] = FuncRep(F
                                                  //| uncRep(SimpleRep(N),provingground.functionfinder.ScalaRep$IdRep@49c5f29d),p
                                                  //| rovingground.functionfinder.ScalaRep$IdRep@62bb4741)
	
	val evalfn = (fn: Long => Term) => fn(1)  //> evalfn  : (Long => provingground.HoTT.Term) => provingground.HoTT.Term = <f
                                                  //| unction1>
	
	val eval = evalrep(evalfn)                //> eval  : provingground.HoTT.FuncObj[provingground.HoTT.FuncObj[provingground
                                                  //| .HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term] = <function1>
	
	val evalout = evalrep.unapply(eval).get   //> evalout  : (Long => provingground.HoTT.Term) => provingground.HoTT.Term = <
                                                  //| function1>
	
	evalout(N.rep(_))                         //> res32: provingground.HoTT.Term = 1
	
	val rfn = r((k: Long) => N.rep(k))        //> rfn  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = <function1>
	
	rfn.asInstanceOf[ExtendedFunction[Term, Long, Term, Term]]
                                                  //> res33: provingground.functionfinder.ScalaRep.ExtendedFunction[provingground
                                                  //| .HoTT.Term,Long,provingground.HoTT.Term,provingground.HoTT.Term] = <functio
                                                  //| n1>
	
	eval(rfn)                                 //> res34: provingground.HoTT.Term = (<function1>(<function1>) : N)
	
	eval(rfn) == N.rep(1)                     //> res35: Boolean = false
	
}