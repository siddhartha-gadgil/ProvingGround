package docsheets

import provingground.HoTT._

object HoTTcore {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //Identity on the universe
  
  val iduniv = lambda("A" :: __)("A" :: __)       //> iduniv  : provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.H
                                                  //| oTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.
                                                  //| HoTT.Term]],provingground.HoTT.Typ[provingground.HoTT.Term] with provinggrou
                                                  //| nd.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]]] = (A⟼A)
 
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
 	
 	val id = lambda(A)(lmbda(a)(a))           //> id  : provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.HoTT.
                                                  //| Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT
                                                  //| .Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggro
                                                  //| und.HoTT.Subs[provingground.HoTT.Term]]] = (A⟼((a : A)⟼(a : A)))
 	
 	//identity on B and its type
 	
 	id(B)                                     //> res1: provingground.HoTT.FuncObj[provingground.HoTT.Term with provingground.
                                                  //| HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term]] = ((a : B)⟼(a : B))
 	
 	id(B).typ                                 //> res2: provingground.HoTT.Typ[provingground.HoTT.Term] = (B⟶B)


// document and check types of generated objects
	id(B) !: B ->: B                          //> res3: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = ((a : B)⟼(a : B))

// Using the dsl to make identity

	val Id = A :~> (a :-> a)                  //> Id  : provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.HoTT.
                                                  //| Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT
                                                  //| .Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term with provinggroun
                                                  //| d.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provinggro
                                                  //| und.HoTT.Subs[provingground.HoTT.Term]]] = (A⟼((a : A)⟼(a : A)))
	
	Id(B) !: B ->: B                          //> res4: provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = ((a : B)⟼(a : B))

// The identity should be a Pi-Type. Note that the type of a term is suppressed if the term is itself a type.
  id.typ                                          //> res5: provingground.HoTT.Typ[provingground.HoTT.Term] = Pi((A⟼(A⟶A)))
  
// Checking the DSL
  Id == id                                        //> res6: Boolean = true
 
 // Types should have type the universe
  (A ->: B).typ                                   //> res7: provingground.HoTT.Universe = _

// Function application

	val f = "a:-> b" :: A ->: B               //> f  : provingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] with provingground.HoTT.Subs[provingground.HoTT.FuncObj[provingground
                                                  //| .HoTT.Term,provingground.HoTT.Term]] = (a:-> b : (A⟶B))
	
	f(a) !: B                                 //> res8: provingground.HoTT.Term = ((a:-> b : (A⟶B))((a : A)) : B)

//Modus Ponens
	val MP = A :~> (
		B :~>
			(a :->
			lmbda(f)(f(a))
			)
			)                         //> MP  : provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.HoTT
                                                  //| .Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.Ho
                                                  //| TT.Term]],provingground.HoTT.FuncTerm[provingground.HoTT.Typ[provingground.
                                                  //| HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provinggroun
                                                  //| d.HoTT.Term]],provingground.HoTT.FuncObj[provingground.HoTT.Term with provi
                                                  //| ngground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.FuncObj[prov
                                                  //| ingground.HoTT.FuncObj[provingground.HoTT.Term,provingground.HoTT.Term] wit
                                                  //| h provingground.HoTT.Subs[provingground.HoTT.FuncObj[provingground.HoTT.Ter
                                                  //| m,provingground.HoTT.Term]],provingground.HoTT.Term]]]] = (A⟼(B⟼((a : A
                                                  //| )⟼((a:-> b : (A⟶B))⟼((a:-> b : (A⟶B))((a : A)) : B)))))
  MP(A)(B).typ                                    //> res9: provingground.HoTT.Typ[provingground.HoTT.Term] = (A⟶((A⟶B)⟶B))
                                                  //| 

// A substitution check
 MP("X" :: __)("Y" :: __).typ                     //> res10: provingground.HoTT.Typ[provingground.HoTT.Term] = (X⟶((X⟶Y)⟶Y)
                                                  //| )

//  A more subtle check - failed
MP(B).typ                                         //> res11: provingground.HoTT.Typ[provingground.HoTT.Term] = Pi((B⟼(B⟶((B�
                                                  //| �B)⟶B))))
MP(B)(A).typ                                      //> res12: provingground.HoTT.Typ[provingground.HoTT.Term] = (A⟶((A⟶A)⟶A)
                                                  //| )
}