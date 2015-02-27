package docsheets

import provingground.HoTT._

import provingground.functionfinder.IntTypes._

import provingground.functionfinder.ScalaRep._



object IntInduc {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val n = N.rep                                   //> n  : provingground.functionfinder.ScalaRep.SimpleRep[Long] = SimpleRep(N)
  
  val three = N.rep(3)                            //> three  : provingground.functionfinder.ScalaRep.SimpleConst[Long] = 3

	val oprep = N.rep -->: N.rep -->: N.rep   //> oprep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Te
                                                  //| rm,Long,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term],Long => Long] = FuncRep(SimpleRep(N),FuncRep(SimpleRep(N),SimpleRep(
                                                  //| N)))
	
	val Nsum = oprep((a: Long) => (b: Long) => a + b)
                                                  //> Nsum  : provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = <function1>

	Nsum(three)(three)                        //> res0: provingground.HoTT.Term = 6

	N.sum                                     //> res1: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Func[provingground.HoTT.Term,provingground.HoTT.Term]] = <function1>

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
	
	val rec =recursion(N)                     //> rec  : provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.H
                                                  //| oTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = <function1>
                                                  //| 
	
	recN                                      //> res6: provingground.HoTT.Lambda[provingground.HoTT.Typ[provingground.HoTT.Te
                                                  //| rm] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.T
                                                  //| erm]],provingground.HoTT.LambdaFixed[provingground.HoTT.Term with provinggro
                                                  //| und.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.LambdaFixed[provin
                                                  //| gground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[prov
                                                  //| ingground.HoTT.Term,provingground.HoTT.Term]] with provingground.HoTT.Subs[p
                                                  //| rovingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func
                                                  //| [provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.functionfi
                                                  //| nder.ScalaRep.SimpleExtendedFunction[provingground.HoTT.Term,Long,provinggro
                                                  //| und.HoTT.Term]]]] = (A|->((a : A)|->((f : (N->(A->A)))|-><function1>)))
	
	
	val f = recN.value.value.variable         //> f  : provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.F
                                                  //| uncObj[provingground.HoTT.Term,provingground.HoTT.Term]] with provingground.
                                                  //| HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.H
                                                  //| oTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (f : (N->(A
                                                  //| ->A)))
	
	changeTyp(f, N ->: N ->: N)               //> res7: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Func[provingground.HoTT.Term,provingground.HoTT.Term]] = (f : (N->(N->N))
                                                  //| )
	
	
	val A = recN.variable                     //> A  : provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT
                                                  //| .Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A
	
	
	f.subs(A, N)                              //> res8: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Func[provingground.HoTT.Term,provingground.HoTT.Term]] = (f : (N->(N->N))
                                                  //| )
	
	recN(N)                                   //> res9: provingground.HoTT.LambdaFixed[provingground.HoTT.Term with provinggro
                                                  //| und.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.LambdaFixed[provin
                                                  //| gground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[prov
                                                  //| ingground.HoTT.Term,provingground.HoTT.Term]] with provingground.HoTT.Subs[p
                                                  //| rovingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func
                                                  //| [provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.functionfi
                                                  //| nder.ScalaRep.SimpleExtendedFunction[provingground.HoTT.Term,Long,provinggro
                                                  //| und.HoTT.Term]]] = ((a : N)|->((f : (N->(N->N)))|-><function1>))
	
	recN(N).value.variable  == f.subs(A, N)   //> res10: Boolean = true
	
	recN(N).typ                               //> res11: provingground.HoTT.Typ[provingground.HoTT.FuncLike[provingground.HoTT
                                                  //| .Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.Ho
                                                  //| TT.LambdaFixed[provingground.HoTT.Func[provingground.HoTT.Term,provinggro
                                                  //| und.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] with prov
                                                  //| ingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provi
                                                  //| ngground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]],pro
                                                  //| vingground.functionfinder.ScalaRep.SimpleExtendedFunction[provingground.HoTT
                                                  //| .Term,Long,provingground.HoTT.Term]]]] = (N->((N->(N->N))->(N->N)))
	
	val recc = recN(N)                        //> recc  : provingground.HoTT.LambdaFixed[provingground.HoTT.Term with provingg
                                                  //| round.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.LambdaFixed[prov
                                                  //| ingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[pr
                                                  //| ovingground.HoTT.Term,provingground.HoTT.Term]] with provingground.HoTT.Subs
                                                  //| [provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.FuncO
                                                  //| bj[provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.function
                                                  //| finder.ScalaRep.SimpleExtendedFunction[provingground.HoTT.Term,Long,provingg
                                                  //| round.HoTT.Term]]] = ((a : N)|->((f : (N->(N->N)))|-><function1>))
	
	recN(N)(N.rep(0))                         //> res12: provingground.HoTT.LambdaFixed[provingground.HoTT.Func[provinggrou
                                                  //| nd.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provinggroun
                                                  //| d.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provin
                                                  //| gground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,proving
                                                  //| ground.HoTT.Term]]],provingground.functionfinder.ScalaRep.SimpleExtendedFunc
                                                  //| tion[provingground.HoTT.Term,Long,provingground.HoTT.Term]] = ((f : (N->(N->
                                                  //| N)))|-><function1>)
	
	// Success :):):)
	recN(N)(N.rep(0))(N.sum)(three)           //> res13: provingground.HoTT.Term = 6
	
	
	
	recN(N)(N.rep(0)).value                   //> res14: provingground.functionfinder.ScalaRep.SimpleExtendedFunction[provingg
                                                  //| round.HoTT.Term,Long,provingground.HoTT.Term] = <function1>
	
	recN(N)(N.rep(0)).variable  == f.replace(A, N)
                                                  //> res15: Boolean = true
	recN(N)(N.rep(0)).value dependsOn (f.subs(A, N))
                                                  //> res16: Boolean = true
	
	val res = recN(N)(N.rep(0)).value.replace(f.subs(A, N), N.sum)
                                                  //> res  : provingground.functionfinder.ScalaRep.SimpleExtendedFunction[proving
                                                  //| ground.HoTT.Term,Long,provingground.HoTT.Term] with provingground.HoTT.Subs
                                                  //| [provingground.functionfinder.ScalaRep.SimpleExtendedFunction[provingground
                                                  //| .HoTT.Term,Long,provingground.HoTT.Term]] = <function1>
	
	res.domrep.unapply(three)                 //> res17: Option[Long] = Some(3)
	
	res.dfn ==  recN(N)(N.rep(0)).value.dfn   //> res18: Boolean = false
	
	val notres = recN(N)(N.rep(0)).value      //> notres  : provingground.functionfinder.ScalaRep.SimpleExtendedFunction[prov
                                                  //| ingground.HoTT.Term,Long,provingground.HoTT.Term] = <function1>
	
	notres.dfn(3)                             //> res19: provingground.HoTT.Term = (((f : (N->(N->N)))(1) : (N->N))((((f : (N
                                                  //| ->(N->N)))(2) : (N->N))((((f : (N->(N->N)))(3) : (N->N))(0) : N)) : N)) : N
                                                  //| )
	
	val ans = res.dfn(3)                      //> ans  : provingground.HoTT.Term = 6
	
	applptnterm.unapply(ans)                  //> res20: Option[(provingground.HoTT.FuncLike[provingground.HoTT.Term,provingg
                                                  //| round.HoTT.Term], provingground.HoTT.Term)] = None
/*
	val arg = applptnterm.unapply(ans).get._1
	
	arg
	
	arg.typ
	
	arg.asInstanceOf[FuncSymb[Term, Term]].name
	
	val argarg = applptnterm.unapply(arg).get._1
	
	argarg replace (A, N)
	
	arg replace(A, N)
	
	res.dfn(3) == notres.dfn(3)
	
	res.dfn(3) == res(three)
	
	res.dfn(3) dependsOn (f.subs(A, N))
	
	res(three)
	
	
	N.sum
	
	recN(N)(N.rep(0)).value.domrep.typ
	
	
	recN(N)(N.rep(0)).value.dfn
	
	val fn = recc(N.rep(0))(N.sum)
	
	fn.domrep
	
	fn.codom
	
	fn.dfn
	
	fn.dfn(0)
	
	fn.dfn(0).typ
	
	fn.dfn(1)
	
	val at1  = fn.dfn(1)
	
	at1.typ
	
	applptnterm.unapply(at1)
	applptnterm.unapply(at1) map (_._2)
	
	
	fn.dfn(3)
	
	fn(N.rep(0))
	
	fn(three)
	
	fn.typ
	
	fn(three).typ
	
	N.rep.unapply(fn(three))

	N.rep.unapply(six)
                    
                    
	
	val add = (k: Long) => (l : Term) => N.sum(N.rep(k))(l)
	
	add(3)(three)
	
	
	val sumto5 = inducFn(n(0), add, 5)
	
	ind(N.rep(0))(add)(5)
	
	val rrr = (n -->: (N -->: N)) -->: (n -->: N)
	
	
	ind(N.rep(0))
  
  
  val x = rrr(ind(N.rep(0)))
	
	x(N.sum)(three)
	
	val in = ind(N.rep(0))(add)
	
	
	
	
	
	
	
	
	
	// Some of the above is completely successful.
	
	// Checking ingredients for recursion
	
	val fnrep = (n -->: N)
  fnrep((k: Long) => N.rep(k))
  
	fnrep(in)(three)

	fnrep(in)(six)
	
	
	// The above is completely successful.
	
	val idrep = n -->: n
	
	idrep((k: Long) => 2 *k)(three)
	
	val addrep = n -->: N -->: N
	
	addrep(add)(three)(three)
	
	val addterm = addrep(add)
	
	val recrep = N -->: addrep -->: fnrep
	
	recrep.domrep
	
	recrep.codomrep
	recrep.domrep.unapply(N.rep(0))
	
	
	
	
	val readd =  addrep.unapply(addterm).get
	readd(3)(six)
	
	recrep(ind)
	
	recrep(ind)(N.rep(0))
	val codrep = addrep -->: fnrep
	
	codrep.unapply(recrep(ind)(N.rep(0)))
	
	fnrep.unapply(recrep(ind)(N.rep(0))(N.sum))
	
	fnrep.unapply(recrep(ind)(N.rep(0))(Nsum))
	
	//
	// This is the crucial problem - the matching for extended functions matched codomrep.
	//
	addrep.unapply(Nsum)
	
	val reind = recrep.unapply(recrep(ind)).get
	
	
	
	recrep(ind).typ

	recrep(ind)(N.rep(0))
	
	val toapply = recrep(ind)(N.rep(0))(N.sum)

	toapply.asInstanceOf[FuncSymb[Term, Term]]


	// The correct sum to use
	recrep(ind)(N.rep(0))(addrep(add))(three)

	// toapply.asInstanceOf[FuncSymb[Term, Term]]

	recrep(ind)(N.rep(0))(N.sum)(N.rep(5))

	recrep(ind)(N.rep(0))(N.sum)(N.rep(5)).typ


	// This is still wrong.
	recrep(ind)(N.rep(0))(N.sum)(three)
	
*/

	// Other stuff that works fine.
	
	val idrep = n -->: n                      //> idrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.T
                                                  //| erm,Long,provingground.HoTT.Term,Long] = FuncRep(SimpleRep(N),SimpleRep(N))
                                                  //| 
	
	idrep((k: Long) => 2 *k)(three)           //> res21: provingground.HoTT.Term = 6

	val NN = IdRep(N)                         //> NN  : provingground.functionfinder.ScalaRep.IdRep[provingground.HoTT.Term] 
                                                  //| = IdRep(N)
	
	NN(three)                                 //> res22: provingground.HoTT.Term = 3
	
	val r = n -->: N                          //> r  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Term,
                                                  //| Long,provingground.HoTT.Term,provingground.HoTT.Term] = FuncRep(SimpleRep(N
                                                  //| ),IdRep(N))
	
	r((k: Long) => N.rep(k))(three)           //> res23: provingground.HoTT.Term = 3

	val rr = N -->: n -->: N                  //> rr  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Term
                                                  //| ,provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term
                                                  //| ,provingground.HoTT.Term],Long => provingground.HoTT.Term] = FuncRep(IdRep(
                                                  //| N),FuncRep(SimpleRep(N),IdRep(N)))
	
	val rsum = rr((x: Term) => (k : Long) => N.sum(x)(N.rep(k)))
                                                  //> rsum  : provingground.HoTT.Func[provingground.HoTT.Term,provingground.Ho
                                                  //| TT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = <function1>

	rsum(three)(three)                        //> res24: provingground.HoTT.Term = 6



	// Test minimal example with domain function : evaluation. This now works.


	val evalrep = (n -->: N) -->: N           //> evalrep  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT
                                                  //| .Func[provingground.HoTT.Term,provingground.HoTT.Term],Long => provinggr
                                                  //| ound.HoTT.Term,provingground.HoTT.Term,provingground.HoTT.Term] = FuncRep(F
                                                  //| uncRep(SimpleRep(N),IdRep(N)),IdRep(N))
	
	val evalfn = (fn: Long => Term) => fn(1)  //> evalfn  : (Long => provingground.HoTT.Term) => provingground.HoTT.Term = <f
                                                  //| unction1>
	
	val eval = evalrep(evalfn)                //> eval  : provingground.HoTT.Func[provingground.HoTT.Func[provingground
                                                  //| .HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term] = <function1>
	
	val evalout = evalrep.unapply(eval).get   //> evalout  : (Long => provingground.HoTT.Term) => provingground.HoTT.Term = <
                                                  //| function1>
	
	evalout(N.rep(_))                         //> res25: provingground.HoTT.Term = 1
	
	val rfn = r((k: Long) => N.rep(k + k *k)) //> rfn  : provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = <function1>
	
	rfn.asInstanceOf[ExtendedFunction[Term, Long, Term, Term]]
                                                  //> res26: provingground.functionfinder.ScalaRep.ExtendedFunction[provingground
                                                  //| .HoTT.Term,Long,provingground.HoTT.Term,provingground.HoTT.Term] = <functio
                                                  //| n1>
	
	eval(rfn)                                 //> res27: provingground.HoTT.Term = 2
	
	eval(rfn) == N.rep(2)                     //> res28: Boolean = true
	
}