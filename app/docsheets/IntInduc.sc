package docsheets

import provingground.HoTT._

import provingground.functionfinder.IntTypes._

import provingground.functionfinder.ScalaRep._

object IntInduc {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val n = N.rep                                   //> n  : provingground.functionfinder.ScalaRep.SimpleRep[Long] = SimpleRep(N)
  
  val three = N.rep(3)                            //> three  : provingground.functionfinder.ScalaRep.SimpleConst[Long] = 3

	val oprep = N.rep -->: N.rep -->: N.rep   //> oprep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Te
                                                  //| rm,Long,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term],Long => Long] = FuncRep(SimpleRep(N),FuncRep(SimpleRep(N),SimpleRep(
                                                  //| N)))
	
	val Nsum = oprep((a: Long) => (b: Long) => a + b)
                                                  //> Nsum  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] = <function1>

	Nsum(three)(three)                        //> res0: provingground.HoTT.Term = 6

	N.sum                                     //> res1: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] = <function1>

	Nsum == N.sum                             //> res2: Boolean = false

	N.sum(three)(three)                       //> res3: provingground.HoTT.Term = 6

	val six = N.sum(three)(three)             //> six  : provingground.HoTT.Term = 6
	
	val nine = N.prod(three)(three)           //> nine  : provingground.HoTT.Term = 9
	
	val induc = inducFn[Term] _               //> induc  : (provingground.HoTT.Term, Long => (provingground.HoTT.Term => provi
                                                  //| ngground.HoTT.Term), Long, provingground.HoTT.Term => provingground.HoTT.Ter
                                                  //| m) => provingground.HoTT.Term = <function4>
	
	val fntyp = N ->: N                       //> fntyp  : provingground.HoTT.FuncTyp[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term] = (N->N)
	
	
	N.rep.typ                                 //> res4: provingground.HoTT.Typ[provingground.HoTT.Term] = N
	
	N.rep.typ ->: N.rep.typ                   //> res5: provingground.HoTT.FuncTyp[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = (N->N)
	
	
	val ind = induccurry[Term]                //> ind  : provingground.HoTT.Term => ((Long => (provingground.HoTT.Term => prov
                                                  //| ingground.HoTT.Term)) => (Long => provingground.HoTT.Term)) = <function1>
	
	val rec =recursion(N)                     //> rec  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT
                                                  //| .FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.H
                                                  //| oTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]] = <function1>
                                                  //| 
	
	recN                                      //> res6: provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.HoTT.
                                                  //| Term],provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]]] = <function1>
                                                  //| 
	
	recN(N)                                   //> res7: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]] = <function1>
	
	recN(N).typ                               //> res8: provingground.HoTT.Typ[provingground.HoTT.Term] = (N->((N->(N->N))->(N
                                                  //| ->N)))
	
	val recc = recN(N)                        //> recc  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.H
                                                  //| oTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.
                                                  //| HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]] = <function1
                                                  //| >
	
	val fn = recc(N.rep(0))(N.sum)            //> fn  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = (<function1>(<function1>) : (N->N))
	
	fn(N.rep(0))                              //> res9: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(0) : N)
                                                  //| 
	
	fn(three)                                 //> res10: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(3) : N
                                                  //| )
	
	fn.typ                                    //> res11: provingground.HoTT.Typ[provingground.HoTT.Term] = (N->N)
	
	fn(three).typ                             //> res12: provingground.HoTT.Typ[provingground.HoTT.Term] = N
	
	N.rep.unapply(fn(three))                  //> res13: Option[Long] = None

	N.rep.unapply(six)                        //> res14: Option[Long] = Some(6)
                    
                    
	
	val add = (k: Long) => (l : Term) => N.sum(N.rep(k))(l)
                                                  //> add  : Long => (provingground.HoTT.Term => provingground.HoTT.Term) = <funct
                                                  //| ion1>
	
	add(3)(three)                             //> res15: provingground.HoTT.Term = 6
	
	
	val sumto5 = inducFn(n(0), add, 5)        //> sumto5  : provingground.HoTT.Term = 15
	
	ind(N.rep(0))(add)(5)                     //> res16: provingground.HoTT.Term = 15
	
	val rrr = (n -->: (N -->: N)) -->: (n -->: N)
                                                  //> rrr  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Fun
                                                  //| cObj[provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.
                                                  //| Term,provingground.HoTT.Term]],Long => (provingground.HoTT.Term => provingg
                                                  //| round.HoTT.Term),provingground.HoTT.FuncObj[provingground.HoTT.Term,proving
                                                  //| ground.HoTT.Term],Long => provingground.HoTT.Term] = FuncRep(FuncRep(Simple
                                                  //| Rep(N),FuncRep(IdRep(N),IdRep(N))),FuncRep(SimpleRep(N),IdRep(N)))
	
	
	ind(N.rep(0))                             //> res17: (Long => (provingground.HoTT.Term => provingground.HoTT.Term)) => (L
                                                  //| ong => provingground.HoTT.Term) = <function1>
  
  
  val x = rrr(ind(N.rep(0)))                      //> x  : provingground.HoTT.FuncObj[provingground.HoTT.FuncObj[provingground.Ho
                                                  //| TT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term]] = <function1>
	
	x(N.sum)(three)                           //> res18: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(3) : 
                                                  //| N)
	
	val in = ind(N.rep(0))(add)               //> in  : Long => provingground.HoTT.Term = <function1>
	
	
	
	
	
	
	
	
	
	// Some of the above is completely successful.
	
	// Checking ingredients for recursion
	
	val fnrep = (n -->: N)                    //> fnrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.T
                                                  //| erm,Long,provingground.HoTT.Term,provingground.HoTT.Term] = FuncRep(SimpleR
                                                  //| ep(N),IdRep(N))
  fnrep((k: Long) => N.rep(k))                    //> res19: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = <function1>
  
	fnrep(in)(three)                          //> res20: provingground.HoTT.Term = 6

	fnrep(in)(six)                            //> res21: provingground.HoTT.Term = 21
	
	
	
	
	val addrep = n -->: N -->: N              //> addrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.
                                                  //| Term,Long,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term],provingground.HoTT.Term => provingground.HoTT.Term] = FuncRep(Si
                                                  //| mpleRep(N),FuncRep(IdRep(N),IdRep(N)))
	
	addrep(add)(three)(three)                 //> res22: provingground.HoTT.Term = 6
	
	val addterm = addrep(add)                 //> addterm  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground
                                                  //| .HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] = <function
                                                  //| 1>
	
	val recrep = N -->: addrep -->: fnrep     //> recrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.
                                                  //| Term,provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.Ho
                                                  //| TT.Term,provingground.HoTT.Term]],provingground.HoTT.FuncObj[provingground.
                                                  //| HoTT.Term,provingground.HoTT.Term]],(Long => (provingground.HoTT.Term => pr
                                                  //| ovingground.HoTT.Term)) => (Long => provingground.HoTT.Term)] = FuncRep(IdR
                                                  //| ep(N),FuncRep(FuncRep(SimpleRep(N),FuncRep(IdRep(N),IdRep(N))),FuncRep(Simp
                                                  //| leRep(N),IdRep(N))))
	
	recrep.domrep                             //> res23: provingground.functionfinder.ScalaRep.ScalaRep[provingground.HoTT.Te
                                                  //| rm,provingground.HoTT.Term] = IdRep(N)
	
	recrep.codomrep                           //> res24: provingground.functionfinder.ScalaRep.ScalaRep[provingground.HoTT.Fu
                                                  //| ncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT
                                                  //| .FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],(Long => (prov
                                                  //| ingground.HoTT.Term => provingground.HoTT.Term)) => (Long => provingground.
                                                  //| HoTT.Term)] = FuncRep(FuncRep(SimpleRep(N),FuncRep(IdRep(N),IdRep(N))),Func
                                                  //| Rep(SimpleRep(N),IdRep(N)))
	recrep.domrep.unapply(N.rep(0))           //> res25: Option[provingground.HoTT.Term] = Some(0)
	
	
	
	
	val readd =  addrep.unapply(addterm).get  //> readd  : Long => (provingground.HoTT.Term => provingground.HoTT.Term) = <fu
                                                  //| nction1>
	readd(3)(six)                             //> res26: provingground.HoTT.Term = 9
	
	recrep(ind)                               //> res27: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provinggroun
                                                  //| d.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]] = <functi
                                                  //| on1>
	
	recrep(ind)(N.rep(0))                     //> res28: provingground.HoTT.FuncObj[provingground.HoTT.FuncObj[provingground.
                                                  //| HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term,provinggroun
                                                  //| d.HoTT.Term]] = <function1>
	val codrep = addrep -->: fnrep            //> codrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.Ho
                                                  //| TT.Term,provingground.HoTT.Term]],Long => (provingground.HoTT.Term => provi
                                                  //| ngground.HoTT.Term),provingground.HoTT.FuncObj[provingground.HoTT.Term,prov
                                                  //| ingground.HoTT.Term],Long => provingground.HoTT.Term] = FuncRep(FuncRep(Sim
                                                  //| pleRep(N),FuncRep(IdRep(N),IdRep(N))),FuncRep(SimpleRep(N),IdRep(N)))
	
	codrep.unapply(recrep(ind)(N.rep(0)))     //> res29: Option[(Long => (provingground.HoTT.Term => provingground.HoTT.Term)
                                                  //| ) => (Long => provingground.HoTT.Term)] = Some(<function1>)
	
	fnrep.unapply(recrep(ind)(N.rep(0))(N.sum))
                                                  //> res30: Option[Long => provingground.HoTT.Term] = None
	
	fnrep.unapply(recrep(ind)(N.rep(0))(Nsum))//> res31: Option[Long => provingground.HoTT.Term] = None
	
	//
	// This is the crucial problem - the matching for extended functions matched codomrep.
	//
	addrep.unapply(Nsum)                      //> res32: Option[Long => (provingground.HoTT.Term => provingground.HoTT.Term)]
                                                  //|  = None
	
	val reind = recrep.unapply(recrep(ind)).get
                                                  //> reind  : provingground.HoTT.Term => ((Long => (provingground.HoTT.Term => p
                                                  //| rovingground.HoTT.Term)) => (Long => provingground.HoTT.Term)) = <function1
                                                  //| >
	
	
	
	recrep(ind).typ                           //> res33: provingground.HoTT.Typ[provingground.HoTT.Term] = (N->((N->(N->N))->
                                                  //| (N->N)))

	recrep(ind)(N.rep(0))                     //> res34: provingground.HoTT.FuncObj[provingground.HoTT.FuncObj[provingground.
                                                  //| HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term,provinggroun
                                                  //| d.HoTT.Term]] = <function1>
	
	val toapply = recrep(ind)(N.rep(0))(N.sum)//> toapply  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground
                                                  //| .HoTT.Term] = (<function1>(<function1>) : (N->N))

	// toapply.asInstanceOf[FuncSymb[Term, Term]]

	recrep(ind)(N.rep(0))(N.sum)(N.rep(5))    //> res35: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(5) : 
                                                  //| N)

	recrep(ind)(N.rep(0))(N.sum)(N.rep(5)).typ//> res36: provingground.HoTT.Typ[provingground.HoTT.Term] = N


	// This is still wrong.
	recrep(ind)(N.rep(0))(N.sum)(three)       //> res37: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(3) : 
                                                  //| N)
	


	// Other stuff that works fine.
	
	val idrep = n -->: n                      //> idrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.T
                                                  //| erm,Long,provingground.HoTT.Term,Long] = FuncRep(SimpleRep(N),SimpleRep(N))
                                                  //| 
	
	idrep((k: Long) => 2 *k)(three)           //> res38: provingground.HoTT.Term = 6

	val NN = IdRep(N)                         //> NN  : provingground.functionfinder.ScalaRep.IdRep[provingground.HoTT.Term] 
                                                  //| = IdRep(N)
	
	NN(three)                                 //> res39: provingground.HoTT.Term = 3
	
	val r = n -->: N                          //> r  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Term,
                                                  //| Long,provingground.HoTT.Term,provingground.HoTT.Term] = FuncRep(SimpleRep(N
                                                  //| ),IdRep(N))
	
	r((k: Long) => N.rep(k))(three)           //> res40: provingground.HoTT.Term = 3

	val rr = N -->: n -->: N                  //> rr  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Term
                                                  //| ,provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term
                                                  //| ,provingground.HoTT.Term],Long => provingground.HoTT.Term] = FuncRep(IdRep(
                                                  //| N),FuncRep(SimpleRep(N),IdRep(N)))
	
	val rsum = rr((x: Term) => (k : Long) => N.sum(x)(N.rep(k)))
                                                  //> rsum  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] = <function1>

	rsum(three)(three)                        //> res41: provingground.HoTT.Term = 6



	// Test minimal example with domain function : evaluation. This now works.


	val evalrep = (n -->: N) -->: N           //> evalrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT
                                                  //| .FuncObj[provingground.HoTT.Term,provingground.HoTT.Term],Long => provinggr
                                                  //| ound.HoTT.Term,provingground.HoTT.Term,provingground.HoTT.Term] = FuncRep(F
                                                  //| uncRep(SimpleRep(N),IdRep(N)),IdRep(N))
	
	val evalfn = (fn: Long => Term) => fn(1)  //> evalfn  : (Long => provingground.HoTT.Term) => provingground.HoTT.Term = <f
                                                  //| unction1>
	
	val eval = evalrep(evalfn)                //> eval  : provingground.HoTT.FuncObj[provingground.HoTT.FuncObj[provingground
                                                  //| .HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term] = <function1>
	
	val evalout = evalrep.unapply(eval).get   //> evalout  : (Long => provingground.HoTT.Term) => provingground.HoTT.Term = <
                                                  //| function1>
	
	evalout(N.rep(_))                         //> res42: provingground.HoTT.Term = 1
	
	val rfn = r((k: Long) => N.rep(k + k *k)) //> rfn  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = <function1>
	
	rfn.asInstanceOf[ExtendedFunction[Term, Long, Term, Term]]
                                                  //> res43: provingground.functionfinder.ScalaRep.ExtendedFunction[provingground
                                                  //| .HoTT.Term,Long,provingground.HoTT.Term,provingground.HoTT.Term] = <functio
                                                  //| n1>
	
	eval(rfn)                                 //> res44: provingground.HoTT.Term = 2
	
	eval(rfn) == N.rep(2)                     //> res45: Boolean = true
	
}