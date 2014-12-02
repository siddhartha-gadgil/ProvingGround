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
	
	recN                                      //> res6: provingground.HoTT.Lambda[provingground.HoTT.Typ[provingground.HoTT.Te
                                                  //| rm] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.T
                                                  //| erm]],provingground.HoTT.LambdaFixed[provingground.HoTT.Term with provinggro
                                                  //| und.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.LambdaFixed[provin
                                                  //| gground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.FuncObj[prov
                                                  //| ingground.HoTT.Term,provingground.HoTT.Term]] with provingground.HoTT.Subs[p
                                                  //| rovingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.FuncObj
                                                  //| [provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.functionfi
                                                  //| nder.ScalaRep.SimpleExtendedFunction[provingground.HoTT.Term,Long,provinggro
                                                  //| und.HoTT.Term]]]] = (A|->((a : A)|->((f : (N->(A->A)))|-><function1>)))
	
	
	val f = recN.value.value.variable         //> f  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.F
                                                  //| uncObj[provingground.HoTT.Term,provingground.HoTT.Term]] with provingground.
                                                  //| HoTT.Subs[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.H
                                                  //| oTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]] = (f : (N->(A
                                                  //| ->A)))
	
	changeTyp(f, N ->: N ->: N)               //> res7: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] = (f : (N->(N->N))
                                                  //| )
	
	
	val A = recN.variable                     //> A  : provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT
                                                  //| .Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A
	
	
	f.subs(A, N)                              //> res8: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] = (f : (N->(N->N))
                                                  //| )
	
	recN(N)                                   //> res9: provingground.HoTT.LambdaFixed[provingground.HoTT.Term with provinggro
                                                  //| und.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.LambdaFixed[provin
                                                  //| gground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.FuncObj[prov
                                                  //| ingground.HoTT.Term,provingground.HoTT.Term]] with provingground.HoTT.Subs[p
                                                  //| rovingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.FuncObj
                                                  //| [provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.functionfi
                                                  //| nder.ScalaRep.SimpleExtendedFunction[provingground.HoTT.Term,Long,provinggro
                                                  //| und.HoTT.Term]]] = ((a : N)|->((f : (N->(N->N)))|-><function1>))
	
	recN(N).value.variable  == f.subs(A, N)   //> res10: Boolean = true
	
	recN(N).typ                               //> res11: provingground.HoTT.Typ[provingground.HoTT.FuncTerm[provingground.HoTT
                                                  //| .Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.Ho
                                                  //| TT.LambdaFixed[provingground.HoTT.FuncObj[provingground.HoTT.Term,provinggro
                                                  //| und.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] with prov
                                                  //| ingground.HoTT.Subs[provingground.HoTT.FuncObj[provingground.HoTT.Term,provi
                                                  //| ngground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]],pro
                                                  //| vingground.functionfinder.ScalaRep.SimpleExtendedFunction[provingground.HoTT
                                                  //| .Term,Long,provingground.HoTT.Term]]]] = (N->((N->(N->N))->(N->N)))
	
	val recc = recN(N)                        //> recc  : provingground.HoTT.LambdaFixed[provingground.HoTT.Term with provingg
                                                  //| round.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.LambdaFixed[prov
                                                  //| ingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.FuncObj[pr
                                                  //| ovingground.HoTT.Term,provingground.HoTT.Term]] with provingground.HoTT.Subs
                                                  //| [provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.FuncO
                                                  //| bj[provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.function
                                                  //| finder.ScalaRep.SimpleExtendedFunction[provingground.HoTT.Term,Long,provingg
                                                  //| round.HoTT.Term]]] = ((a : N)|->((f : (N->(N->N)))|-><function1>))
	
	recN(N)(N.rep(0))                         //> res12: provingground.HoTT.LambdaFixed[provingground.HoTT.FuncObj[provinggrou
                                                  //| nd.HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term,provinggroun
                                                  //| d.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.FuncObj[provin
                                                  //| gground.HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term,proving
                                                  //| ground.HoTT.Term]]],provingground.functionfinder.ScalaRep.SimpleExtendedFunc
                                                  //| tion[provingground.HoTT.Term,Long,provingground.HoTT.Term]] = ((f : (N->(N->
                                                  //| N)))|-><function1>)
	
	recN(N)(N.rep(0)).value                   //> res13: provingground.functionfinder.ScalaRep.SimpleExtendedFunction[provingg
                                                  //| round.HoTT.Term,Long,provingground.HoTT.Term] = <function1>
	
	recN(N)(N.rep(0)).variable  == f.replace(A, N)
                                                  //> res14: Boolean = true
	recN(N)(N.rep(0)).value dependsOn (f.subs(A, N))
                                                  //> res15: Boolean = true
	
	val res = recN(N)(N.rep(0)).value.replace(f.subs(A, N), N.sum)
                                                  //> res  : provingground.functionfinder.ScalaRep.SimpleExtendedFunction[proving
                                                  //| ground.HoTT.Term,Long,provingground.HoTT.Term] with provingground.HoTT.Subs
                                                  //| [provingground.functionfinder.ScalaRep.SimpleExtendedFunction[provingground
                                                  //| .HoTT.Term,Long,provingground.HoTT.Term]] = <function1>
	
	res.domrep.unapply(three)                 //> res16: Option[Long] = Some(3)
	
	res.dfn ==  recN(N)(N.rep(0)).value.dfn   //> res17: Boolean = false
	
	val notres = recN(N)(N.rep(0)).value      //> notres  : provingground.functionfinder.ScalaRep.SimpleExtendedFunction[prov
                                                  //| ingground.HoTT.Term,Long,provingground.HoTT.Term] = <function1>
	
	notres.dfn(3)                             //> res18: provingground.HoTT.Term = (((f : (N->(A->A)))(1) : (N->N))((((f : (N
                                                  //| ->(A->A)))(2) : (N->N))((((f : (N->(A->A)))(3) : (N->N))(0) : N)) : N)) : N
                                                  //| )
	
	val ans = res.dfn(3)                      //> ans  : provingground.HoTT.Term = (((f : (N->(A->A)))(1) : (N->N))((((f : (N
                                                  //| ->(A->A)))(2) : (N->N))((((f : (N->(A->A)))(3) : (N->N))(0) : N)) : N)) : N
                                                  //| )
	
	applptnterm.unapply(ans)                  //> res19: Option[(provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingg
                                                  //| round.HoTT.Term], provingground.HoTT.Term)] = Some((((f : (N->(A->A)))(1) :
                                                  //|  (N->N)),(((f : (N->(A->A)))(2) : (N->N))((((f : (N->(A->A)))(3) : (N->N))(
                                                  //| 0) : N)) : N)))
	
	val arg = applptnterm.unapply(ans).get._1 //> arg  : provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term] = ((f : (N->(A->A)))(1) : (N->N))
	
	arg                                       //> res20: provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term] = ((f : (N->(A->A)))(1) : (N->N))
	
	arg.typ                                   //> res21: provingground.HoTT.Typ[provingground.HoTT.Term] = (N->N)
	
	arg.asInstanceOf[FuncSymb[Term, Term]].name
                                                  //> res22: provingground.HoTT.AnySym = (f : (N->(A->A)))(1)
	
	val argarg = applptnterm.unapply(arg).get._1
                                                  //> argarg  : provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground
                                                  //| .HoTT.Term] = (f : (N->(A->A)))
	
	argarg replace (A, N)                     //> res23: provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term] with provingground.HoTT.Subs[provingground.HoTT.FuncTerm[provinggr
                                                  //| ound.HoTT.Term,provingground.HoTT.Term]] = (f : (N->(N->N)))
	
	arg replace(A, N)                         //> res24: provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term] with provingground.HoTT.Subs[provingground.HoTT.FuncTerm[provinggr
                                                  //| ound.HoTT.Term,provingground.HoTT.Term]] = ((f : (N->(A->A)))(1) : (N->N))
	
	res.dfn(3) == notres.dfn(3)               //> res25: Boolean = true
	
	res.dfn(3) == res(three)                  //> res26: Boolean = true
	
	res.dfn(3) dependsOn (f.subs(A, N))       //> res27: Boolean = false
	
	res(three)                                //> res28: provingground.HoTT.Term = (((f : (N->(A->A)))(1) : (N->N))((((f : (N
                                                  //| ->(A->A)))(2) : (N->N))((((f : (N->(A->A)))(3) : (N->N))(0) : N)) : N)) : N
                                                  //| )
	
	
	N.sum                                     //> res29: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] = <function1>
	
	recN(N)(N.rep(0)).value.domrep.typ        //> res30: provingground.HoTT.Typ[provingground.HoTT.Term] = N
	
	
	recN(N)(N.rep(0)).value.dfn               //> res31: Long => provingground.HoTT.Term = <function1>
	
	val fn = recc(N.rep(0))(N.sum)            //> fn  : provingground.functionfinder.ScalaRep.SimpleExtendedFunction[provingg
                                                  //| round.HoTT.Term,Long,provingground.HoTT.Term] = <function1>
	
	fn.domrep                                 //> res32: provingground.functionfinder.ScalaRep.ScalaRep[provingground.HoTT.Te
                                                  //| rm,Long] = SimpleRep(N)
	
	fn.codom                                  //> res33: provingground.HoTT.Typ[provingground.HoTT.Term] = N
	
	fn.dfn                                    //> res34: Long => provingground.HoTT.Term = <function1>
	
	fn.dfn(0)                                 //> res35: provingground.HoTT.Term = 0
	
	fn.dfn(0).typ                             //> res36: provingground.HoTT.Typ[provingground.HoTT.Term] = N
	
	fn.dfn(1)                                 //> res37: provingground.HoTT.Term = (((f : (N->(A->A)))(1) : (N->N))(0) : N)
	
	val at1  = fn.dfn(1)                      //> at1  : provingground.HoTT.Term = (((f : (N->(A->A)))(1) : (N->N))(0) : N)
	
	at1.typ                                   //> res38: provingground.HoTT.Typ[provingground.HoTT.Term] = N
	
	applptnterm.unapply(at1)                  //> res39: Option[(provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingg
                                                  //| round.HoTT.Term], provingground.HoTT.Term)] = Some((((f : (N->(A->A)))(1) :
                                                  //|  (N->N)),0))
	applptnterm.unapply(at1) map (_._2)       //> res40: Option[provingground.HoTT.Term] = Some(0)
	
	
	fn.dfn(3)                                 //> res41: provingground.HoTT.Term = (((f : (N->(A->A)))(1) : (N->N))((((f : (N
                                                  //| ->(A->A)))(2) : (N->N))((((f : (N->(A->A)))(3) : (N->N))(0) : N)) : N)) : N
                                                  //| )
	
	fn(N.rep(0))                              //> res42: provingground.HoTT.Term = 0
	
	fn(three)                                 //> res43: provingground.HoTT.Term = (((f : (N->(A->A)))(1) : (N->N))((((f : (N
                                                  //| ->(A->A)))(2) : (N->N))((((f : (N->(A->A)))(3) : (N->N))(0) : N)) : N)) : N
                                                  //| )
	
	fn.typ                                    //> res44: provingground.HoTT.FuncTyp[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = (N->N)
	
	fn(three).typ                             //> res45: provingground.HoTT.Typ[provingground.HoTT.Term] = N
	
	N.rep.unapply(fn(three))                  //> res46: Option[Long] = None

	N.rep.unapply(six)                        //> res47: Option[Long] = Some(6)
                    
                    
	
	val add = (k: Long) => (l : Term) => N.sum(N.rep(k))(l)
                                                  //> add  : Long => (provingground.HoTT.Term => provingground.HoTT.Term) = <func
                                                  //| tion1>
	
	add(3)(three)                             //> res48: provingground.HoTT.Term = 6
	
	
	val sumto5 = inducFn(n(0), add, 5)        //> sumto5  : provingground.HoTT.Term = 15
	
	ind(N.rep(0))(add)(5)                     //> res49: provingground.HoTT.Term = 15
	
	val rrr = (n -->: (N -->: N)) -->: (n -->: N)
                                                  //> rrr  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Fun
                                                  //| cObj[provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.
                                                  //| Term,provingground.HoTT.Term]],Long => (provingground.HoTT.Term => provingg
                                                  //| round.HoTT.Term),provingground.HoTT.FuncObj[provingground.HoTT.Term,proving
                                                  //| ground.HoTT.Term],Long => provingground.HoTT.Term] = FuncRep(FuncRep(Simple
                                                  //| Rep(N),FuncRep(IdRep(N),IdRep(N))),FuncRep(SimpleRep(N),IdRep(N)))
	
	
	ind(N.rep(0))                             //> res50: (Long => (provingground.HoTT.Term => provingground.HoTT.Term)) => (L
                                                  //| ong => provingground.HoTT.Term) = <function1>
  
  
  val x = rrr(ind(N.rep(0)))                      //> x  : provingground.HoTT.FuncObj[provingground.HoTT.FuncObj[provingground.Ho
                                                  //| TT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term]] = <function1>
	
	x(N.sum)(three)                           //> res51: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(3) : 
                                                  //| N)
	
	val in = ind(N.rep(0))(add)               //> in  : Long => provingground.HoTT.Term = <function1>
	
	
	
	
	
	
	
	
	
	// Some of the above is completely successful.
	
	// Checking ingredients for recursion
	
	val fnrep = (n -->: N)                    //> fnrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.T
                                                  //| erm,Long,provingground.HoTT.Term,provingground.HoTT.Term] = FuncRep(SimpleR
                                                  //| ep(N),IdRep(N))
  fnrep((k: Long) => N.rep(k))                    //> res52: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = <function1>
  
	fnrep(in)(three)                          //> res53: provingground.HoTT.Term = 6

	fnrep(in)(six)                            //> res54: provingground.HoTT.Term = 21
	
	
	// The above is completely successful.
	
	val idrep = n -->: n                      //> idrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.T
                                                  //| erm,Long,provingground.HoTT.Term,Long] = FuncRep(SimpleRep(N),SimpleRep(N))
                                                  //| 
	
	idrep((k: Long) => 2 *k)(three)           //> res55: provingground.HoTT.Term = 6
	
	val addrep = n -->: N -->: N              //> addrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.
                                                  //| Term,Long,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term],provingground.HoTT.Term => provingground.HoTT.Term] = FuncRep(Si
                                                  //| mpleRep(N),FuncRep(IdRep(N),IdRep(N)))
	
	addrep(add)(three)(three)                 //> res56: provingground.HoTT.Term = 6
	
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
	
	recrep.domrep                             //> res57: provingground.functionfinder.ScalaRep.ScalaRep[provingground.HoTT.Te
                                                  //| rm,provingground.HoTT.Term] = IdRep(N)
	
	recrep.codomrep                           //> res58: provingground.functionfinder.ScalaRep.ScalaRep[provingground.HoTT.Fu
                                                  //| ncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT
                                                  //| .FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],(Long => (prov
                                                  //| ingground.HoTT.Term => provingground.HoTT.Term)) => (Long => provingground.
                                                  //| HoTT.Term)] = FuncRep(FuncRep(SimpleRep(N),FuncRep(IdRep(N),IdRep(N))),Func
                                                  //| Rep(SimpleRep(N),IdRep(N)))
	recrep.domrep.unapply(N.rep(0))           //> res59: Option[provingground.HoTT.Term] = Some(0)
	
	
	
	
	val readd =  addrep.unapply(addterm).get  //> readd  : Long => (provingground.HoTT.Term => provingground.HoTT.Term) = <fu
                                                  //| nction1>
	readd(3)(six)                             //> res60: provingground.HoTT.Term = 9
	
	recrep(ind)                               //> res61: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.FuncObj[provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]],provinggroun
                                                  //| d.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]]] = <functi
                                                  //| on1>
	
	recrep(ind)(N.rep(0))                     //> res62: provingground.HoTT.FuncObj[provingground.HoTT.FuncObj[provingground.
                                                  //| HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term,provinggroun
                                                  //| d.HoTT.Term]] = <function1>
	val codrep = addrep -->: fnrep            //> codrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.
                                                  //| FuncObj[provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.Ho
                                                  //| TT.Term,provingground.HoTT.Term]],Long => (provingground.HoTT.Term => provi
                                                  //| ngground.HoTT.Term),provingground.HoTT.FuncObj[provingground.HoTT.Term,prov
                                                  //| ingground.HoTT.Term],Long => provingground.HoTT.Term] = FuncRep(FuncRep(Sim
                                                  //| pleRep(N),FuncRep(IdRep(N),IdRep(N))),FuncRep(SimpleRep(N),IdRep(N)))
	
	codrep.unapply(recrep(ind)(N.rep(0)))     //> res63: Option[(Long => (provingground.HoTT.Term => provingground.HoTT.Term)
                                                  //| ) => (Long => provingground.HoTT.Term)] = Some(<function1>)
	
	fnrep.unapply(recrep(ind)(N.rep(0))(N.sum))
                                                  //> res64: Option[Long => provingground.HoTT.Term] = None
	
	fnrep.unapply(recrep(ind)(N.rep(0))(Nsum))//> res65: Option[Long => provingground.HoTT.Term] = None
	
	//
	// This is the crucial problem - the matching for extended functions matched codomrep.
	//
	addrep.unapply(Nsum)                      //> res66: Option[Long => (provingground.HoTT.Term => provingground.HoTT.Term)]
                                                  //|  = None
	
	val reind = recrep.unapply(recrep(ind)).get
                                                  //> reind  : provingground.HoTT.Term => ((Long => (provingground.HoTT.Term => p
                                                  //| rovingground.HoTT.Term)) => (Long => provingground.HoTT.Term)) = <function1
                                                  //| >
	
	
	
	recrep(ind).typ                           //> res67: provingground.HoTT.Typ[provingground.HoTT.Term] = (N->((N->(N->N))->
                                                  //| (N->N)))

	recrep(ind)(N.rep(0))                     //> res68: provingground.HoTT.FuncObj[provingground.HoTT.FuncObj[provingground.
                                                  //| HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term,provinggroun
                                                  //| d.HoTT.Term]] = <function1>
	
	val toapply = recrep(ind)(N.rep(0))(N.sum)//> toapply  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground
                                                  //| .HoTT.Term] = (<function1>(<function1>) : (N->N))

	toapply.asInstanceOf[FuncSymb[Term, Term]]//> res69: provingground.HoTT.FuncSymb[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Term] = (<function1>(<function1>) : (N->N))


	// The correct sum to use
	recrep(ind)(N.rep(0))(addrep(add))(three) //> res70: provingground.HoTT.Term = 6

	// toapply.asInstanceOf[FuncSymb[Term, Term]]

	recrep(ind)(N.rep(0))(N.sum)(N.rep(5))    //> res71: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(5) : 
                                                  //| N)

	recrep(ind)(N.rep(0))(N.sum)(N.rep(5)).typ//> res72: provingground.HoTT.Typ[provingground.HoTT.Term] = N


	// This is still wrong.
	recrep(ind)(N.rep(0))(N.sum)(three)       //> res73: provingground.HoTT.Term = ((<function1>(<function1>) : (N->N))(3) : 
                                                  //| N)
	


	// Other stuff that works fine.
	
//	val idrep = n -->: n
	
	idrep((k: Long) => 2 *k)(three)           //> res74: provingground.HoTT.Term = 6

	val NN = IdRep(N)                         //> NN  : provingground.functionfinder.ScalaRep.IdRep[provingground.HoTT.Term] 
                                                  //| = IdRep(N)
	
	NN(three)                                 //> res75: provingground.HoTT.Term = 3
	
	val r = n -->: N                          //> r  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Term,
                                                  //| Long,provingground.HoTT.Term,provingground.HoTT.Term] = FuncRep(SimpleRep(N
                                                  //| ),IdRep(N))
	
	r((k: Long) => N.rep(k))(three)           //> res76: provingground.HoTT.Term = 3

	val rr = N -->: n -->: N                  //> rr  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Term
                                                  //| ,provingground.HoTT.Term,provingground.HoTT.FuncObj[provingground.HoTT.Term
                                                  //| ,provingground.HoTT.Term],Long => provingground.HoTT.Term] = FuncRep(IdRep(
                                                  //| N),FuncRep(SimpleRep(N),IdRep(N)))
	
	val rsum = rr((x: Term) => (k : Long) => N.sum(x)(N.rep(k)))
                                                  //> rsum  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term]] = <function1>

	rsum(three)(three)                        //> res77: provingground.HoTT.Term = 6



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
	
	evalout(N.rep(_))                         //> res78: provingground.HoTT.Term = 1
	
	val rfn = r((k: Long) => N.rep(k + k *k)) //> rfn  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = <function1>
	
	rfn.asInstanceOf[ExtendedFunction[Term, Long, Term, Term]]
                                                  //> res79: provingground.functionfinder.ScalaRep.ExtendedFunction[provingground
                                                  //| .HoTT.Term,Long,provingground.HoTT.Term,provingground.HoTT.Term] = <functio
                                                  //| n1>
	
	eval(rfn)                                 //> res80: provingground.HoTT.Term = 2
	
	eval(rfn) == N.rep(2)                     //> res81: Boolean = true
	
}