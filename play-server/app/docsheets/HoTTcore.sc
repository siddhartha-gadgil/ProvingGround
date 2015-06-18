package docsheets

import provingground.HoTT._

object HoTTcore {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
   
  //Identity on the universe
  
  val iduniv = lambda("A" :: __)("A" :: __)       //> iduniv  : provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.H
                                                  //| oTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.
                                                  //| HoTT.Term]],provingground.HoTT.Typ[provingground.HoTT.Term] with provinggrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]]] = (A|->A)
 
 	// Some variable types for convenience. Note that such variables should not be generated as they represent possibly empty types.
 	
 	val A = "A" :: __                         //> A  : provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT
                                                  //| .Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A
 	
 	val B = "B" :: __                         //> B  : provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT
                                                  //| .Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = B
 	
 	// Apply identity on universe to the type B.
 	iduniv(B)                                 //> res0: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoT
                                                  //| T.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = B
 	
 	//Now a full fledged identity, using a new variable for convenience.
 	val a ="a" :: A                           //> a  : provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT
                                                  //| .Term] = (a : A)
 	
 	val id = lambda(A)(lmbda(a)(a))           //> id  : provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.HoTT.
                                                  //| Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT
                                                  //| .Term]],provingground.HoTT.Func[provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggro
                                                  //| und.HoTT.Subs[provingground.HoTT.Term]]] = (A|->(((a : A) : A)|->((a : A) : 
                                                  //| A)))
 	
 	//identity on B and its type
 	
 	id(B)                                     //> res1: provingground.HoTT.Func[provingground.HoTT.Term with provingground.
                                                  //| HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term]] = (((a : A) : B)|->((a : A) : B))
 	
 	id(B).typ                                 //> res2: provingground.HoTT.Typ[provingground.HoTT.Term] = (B->B)


// document and check types of generated objects
	id(B) !: B ->: B                          //> res3: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = (((a : A) : B)|->((a : A) : B))

// Using the dsl to make identity

	val Id = A :~> (a :-> a)                  //> Id  : provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.HoTT.
                                                  //| Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT
                                                  //| .Term]],provingground.HoTT.Func[provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggro
                                                  //| und.HoTT.Subs[provingground.HoTT.Term]]] = (A|->(((a : A) : A)|->((a : A) : 
                                                  //| A)))
	
	Id(B) !: B ->: B                          //> res4: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = (((a : A) : B)|->((a : A) : B))

// The identity should be a Pi-Type. Note that the type of a term is suppressed if the term is itself a type.
  id.typ                                          //> res5: provingground.HoTT.Typ[provingground.HoTT.Term] = (_->(A->A))
  
// Checking the DSL - note that lambdas are now essentailly never definitionally equal
  Id == id                                        //> res6: Boolean = true
 
  Id(A)(a) == id(A)(a)                            //> res7: Boolean = true
 
 // Types should have type the universe
  (A ->: B).typ                                   //> res8: provingground.HoTT.Universe = _

// Function application

	val f = "a:-> b" :: A ->: B               //> f  : provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground
                                                  //| .HoTT.Term,provingground.HoTT.Term]] = (a:-> b : (A->B))
	
	f(a) !: B                                 //> res9: provingground.HoTT.Term = ((a:-> b : (A->B))((a : A)) : B)

//Modus Ponens
	val MP = A :~> (
		B :~>
			(a :->
			lmbda(f)(f(a))
			)
			)                         //> MP  : provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.HoTT
                                                  //| .Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.Ho
                                                  //| TT.Term]],provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.
                                                  //| HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provinggroun
                                                  //| d.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term with provi
                                                  //| ngground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[prov
                                                  //| ingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] wit
                                                  //| h provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Ter
                                                  //| m,provingground.HoTT.Term]],provingground.HoTT.Term]]]] = (A|->(B|->(((a : 
                                                  //| A) : A)|->((provingground.HoTT$Typ$newname$2$@57a8551 : (A->B))|->((proving
                                                  //| ground.HoTT$Typ$newname$2$@57a8551 : (A->B))(((a : A) : A)) : B)))))
  MP(A)(B).typ                                    //> res10: provingground.HoTT.Typ[provingground.HoTT.Term] = (A->((A->B)->B))

// A substitution check
 MP("X" :: __)("Y" :: __).typ                     //> res11: provingground.HoTT.Typ[provingground.HoTT.Term] = (X->((X->Y)->Y))

//  A more subtle check - works now :)
MP(B).typ                                         //> res12: provingground.HoTT.Typ[provingground.HoTT.Term] = (_->(B->((B->B)->B
                                                  //| )))
MP(B)(A).typ                                      //> res13: provingground.HoTT.Typ[provingground.HoTT.Term] = (B->((B->A)->A))
	Id(A) == id(A)                            //> res14: Boolean = true
}