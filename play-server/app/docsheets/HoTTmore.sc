package docsheets

import provingground.HoTT._

object HoTTmore {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val A = "A" :: Type                               //> A  : provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT
                                                  //| .Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A
  
  val B = "B" :: Type                               //> B  : provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT
                                                  //| .Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = B
  
  val AB = pair(A, B)                             //> AB  : provingground.HoTT.PairTyp[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = PairTyp(A,B)

	val x = "x" :: AB                         //> x  : provingground.HoTT.PairObj[provingground.HoTT.Term,provingground.HoTT.T
                                                  //| erm] with provingground.HoTT.Subs[provingground.HoTT.PairObj[provingground.H
                                                  //| oTT.Term,provingground.HoTT.Term]] = PairObj((x_1 : A),(x_2 : B))
  AB dependsOn A                                  //> res0: Boolean = true

	A dependsOn A                             //> res1: Boolean = true
	
	AB dependsOn AB                           //> res2: Boolean = true

	A dependsOn B                             //> res3: Boolean = false

	(A ->: B) dependsOn A                     //> res4: Boolean = true
	
	(A ->: B) dependsOn B                     //> res5: Boolean = true

	(A ->: B) dependsOn ("C" :: Type)           //> res6: Boolean = false
	
	val a = "a" :: A                          //> a  : provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT
                                                  //| .Term] = (a : A)
	
	val b = "b" :: B                          //> b  : provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT
                                                  //| .Term] = (b : B)
	
	pair(a, b) !: AB                          //> res7: provingground.HoTT.PairObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = PairObj((a : A),(b : B))


	pair(a, b) dependsOn a                    //> res8: Boolean = false
	
	pair(a, b) subs(a, "aa" :: A)             //> res9: provingground.HoTT.PairObj[provingground.HoTT.Term with provingground.
                                                  //| HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term]] = PairObj((a : A),(b : B))

	pair(a, b) subs(a, b)                     //> res10: provingground.HoTT.PairObj[provingground.HoTT.Term with provingground
                                                  //| .HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Term]] = PairObj((a : A),(b : B))
	pair(a, b) replace (a, "aa" :: A)         //> res11: provingground.HoTT.PairObj[provingground.HoTT.Term with provingground
                                                  //| .HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingg
                                                  //| round.HoTT.PairObj[provingground.HoTT.Term with provingground.HoTT.Subs[prov
                                                  //| ingground.HoTT.Term],provingground.HoTT.Term with provingground.HoTT.Subs[pr
                                                  //| ovingground.HoTT.Term]]] = PairObj((a : A),(b : B))
	
	a replace (pair(a, b), pair("aa" :: A, b :: B))
                                                  //> res12: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.Ho
                                                  //| TT.Term] = (a : A)
	
	
	// Fails because of type mismatch
	//pair(a, b) replace (a, b)
 
 
  IdentityTyp(A, a, a)                            //> res13: provingground.HoTT.IdentityTyp[provingground.HoTT.Term with provinggr
                                                  //| ound.HoTT.Subs[provingground.HoTT.Term]] = IdentityTyp(A,(a : A),(a : A))
  
  val ids = a :-> (IdentityTyp(A, a, a))          //> ids  : provingground.HoTT.Func[provingground.HoTT.Term with provingground
                                                  //| .HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.IdentityTyp[provinggr
                                                  //| ound.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]]] = (((
                                                  //| a : A) : A)|->IdentityTyp(A,(a : A),(a : A)))
 
	val idA = a :-> (a =:= a)                 //> idA  : provingground.HoTT.Func[provingground.HoTT.Term with provingground
                                                  //| .HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.IdentityTyp[provinggr
                                                  //| ound.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]]] = (((
                                                  //| a : A) : A)|->IdentityTyp(A,(a : A),(a : A)))
	
	ids == idA                                //> res14: Boolean = true

	SigmaTyp(idA)                             //> res15: provingground.HoTT.SigmaTyp[provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = S((((a : A) 
                                                  //| : A)|->IdentityTyp(A,(a : A),(a : A))))

	val ab = pair(a, b)                       //> ab  : provingground.HoTT.PairObj[provingground.HoTT.Term with provingground.
                                                  //| HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term]] = PairObj((a : A),(b : B))
	
	val idp = lambda(ab)(ab)                  //> idp  : provingground.HoTT.FuncLike[provingground.HoTT.PairObj[provingground.
                                                  //| HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provinggroun
                                                  //| d.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]],provinggr
                                                  //| ound.HoTT.PairObj[provingground.HoTT.Term with provingground.HoTT.Subs[provi
                                                  //| ngground.HoTT.Term],provingground.HoTT.Term with provingground.HoTT.Subs[pro
                                                  //| vingground.HoTT.Term]]] = (PairObj(((a : A) : A),((b : B) : B))|->PairObj((a
                                                  //|  : A),(b : B)))
	idp(ab)                                   //> res16: provingground.HoTT.PairObj[provingground.HoTT.Term with provingground
                                                  //| .HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Term]] = PairObj((a : A),(b : B))

	idp("x" :: pair(A, B))                    //> res17: provingground.HoTT.PairObj[provingground.HoTT.Term with provingground
                                                  //| .HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Term]] = PairObj((a : A),(b : B))
	
	val p = lambda(pair(a, b))(a)             //> p  : provingground.HoTT.FuncLike[provingground.HoTT.PairObj[provingground.Ho
                                                  //| TT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.
                                                  //| HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]],provinggrou
                                                  //| nd.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]] = (PairO
                                                  //| bj(((a : A) : A),((b : B) : B))|->(a : A))
	p("x" :: pair(A, B))                      //> res18: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.Ho
                                                  //| TT.Term] = (a : A)
	p.dom                                     //> res19: provingground.HoTT.Typ[provingground.HoTT.Term] = PairTyp(A,B)


	val pp = Lambda(ab, a)                    //> pp  : provingground.HoTT.Lambda[provingground.HoTT.PairObj[provingground.HoT
                                                  //| T.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.H
                                                  //| oTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]],provinggroun
                                                  //| d.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]] = (PairOb
                                                  //| j((a : A),(b : B))|->(a : A))
  pp("x" :: pair(A, B))                           //> res20: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.Ho
                                                  //| TT.Term] = (a : A)

	val aabb = innervar(ab)                   //> aabb  : provingground.HoTT.PairObj[provingground.HoTT.Term with provinggrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provingg
                                                  //| round.HoTT.Subs[provingground.HoTT.Term]] = PairObj((PairObj((a : A),(b : B
                                                  //| )) : A),(PairObj((a : A),(b : B)) : B))

	ab dependsOn ab                           //> res21: Boolean = false

	ab dependsOn a                            //> res22: Boolean = false


	AB.asInstanceOf[PairTyp[Term, Term]]      //> res23: provingground.HoTT.PairTyp[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = PairTyp(A,B)
	
	AB dependsOn AB                           //> res24: Boolean = true
	
	AB dependsOn A                            //> res25: Boolean = true
	
	A dependsOn AB                            //> res26: Boolean = true
	
	AB.obj                                    //> res27: provingground.HoTT.PairObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = PairObj((provingground.HoTT$Typ$newname$2$@202c9041_1 : A),(provi
                                                  //| ngground.HoTT$Typ$newname$2$@202c9041_2 : B))
	
	a dependsOn a                             //> res28: Boolean = false
	
	pair(a, b) replace (a, "aa" :: A)         //> res29: provingground.HoTT.PairObj[provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggr
                                                  //| ound.HoTT.Subs[provingground.HoTT.Term]] with provingground.HoTT.Subs[provi
                                                  //| ngground.HoTT.PairObj[provingground.HoTT.Term with provingground.HoTT.Subs[
                                                  //| provingground.HoTT.Term],provingground.HoTT.Term with provingground.HoTT.Su
                                                  //| bs[provingground.HoTT.Term]]] = PairObj((a : A),(b : B))
	
	
	val fst = Lambda(x, x.first)              //> fst  : provingground.HoTT.Lambda[provingground.HoTT.PairObj[provingground.H
                                                  //| oTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provinggroun
                                                  //| d.HoTT.PairObj[provingground.HoTT.Term,provingground.HoTT.Term]],provinggro
                                                  //| und.HoTT.Term] = (PairObj((x_1 : A),(x_2 : B))|->(x_1 : A))
	fst.variable                              //> res30: provingground.HoTT.PairObj[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] with provingground.HoTT.Subs[provingground.HoTT.PairObj[provinggrou
                                                  //| nd.HoTT.Term,provingground.HoTT.Term]] = PairObj((x_1 : A),(x_2 : B))
	
	val fstl = lambda(x)(x.first)             //> fstl  : provingground.HoTT.FuncLike[provingground.HoTT.PairObj[provinggroun
                                                  //| d.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provinggr
                                                  //| ound.HoTT.PairObj[provingground.HoTT.Term,provingground.HoTT.Term]],proving
                                                  //| ground.HoTT.Term] = (PairObj(((x_1 : A) : A),((x_2 : B) : B))|->(x_1 : A))
	
	
	fstl(pair(a, b))                          //> res31: provingground.HoTT.Term = (x_1 : A)
	
	fst.variable == x                         //> res32: Boolean = true
	
	fst.value                                 //> res33: provingground.HoTT.Term = (x_1 : A)
	
	fst(pair(a, b))                           //> res34: provingground.HoTT.Term = (x_1 : A)
	
	idp(pair(a, b))                           //> res35: provingground.HoTT.PairObj[provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggr
                                                  //| ound.HoTT.Subs[provingground.HoTT.Term]] = PairObj((a : A),(b : B))
}