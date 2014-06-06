package main.scala
import provingGround.HoTT._


object HoTTExperiment {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val A = "A" ::__                                //> A  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = (A : Universe(0))
  val a = "a" :: A                                //> a  : provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoTT
                                                  //| .Term] = (a : (A : Universe(0)))
  lambda(a)(a)                                    //> res0: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term with provingGround
                                                  //| .HoTT.Subs[provingGround.HoTT.Term],provingGround.HoTT.Term with provingGrou
                                                  //| nd.HoTT.Subs[provingGround.HoTT.Term]] = <function1>
  TermOps(a) :-> a                                //> res1: provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT
                                                  //| .Term with provingGround.HoTT.Subs[provingGround.HoTT.Term]] = <function1>
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
                                                  //| ound.HoTT.Subs[provingGround.HoTT.Term]]] = <function1>
   
  Id.typ                                          //> res2: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (Universe(0) -> ((A 
                                                  //| : Universe(0)) -> (A : Universe(0))))
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
                                                  //| ,provingGround.HoTT.Term]],provingGround.HoTT.Term]]]] = <function1>
  MPall.typ                                       //> res3: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (Universe(0) -> (Uni
                                                  //| verse(0) -> ((A : Universe(0)) -> (((A : Universe(0)) -> (B : Universe(0))) 
                                                  //| -> (B : Universe(0))))))
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
                                                  //| gGround.HoTT.Term]],provingGround.HoTT.Term]] = <function1>
	MP.typ                                    //> res4: provingGround.HoTT.Typ[provingGround.HoTT.Term] = ((A : Universe(0)) -
                                                  //| > (((A : Universe(0)) -> (B : Universe(0))) -> (B : Universe(0))))

	val X = "X" :: __                         //> X  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = (X : Universe(0))
	val Y = "Y" :: __                         //> Y  : provingGround.HoTT.Typ[provingGround.HoTT.Term] with provingGround.HoTT
                                                  //| .Subs[provingGround.HoTT.Typ[provingGround.HoTT.Term]] = (Y : Universe(0))
			val x = "x" :: X          //> x  : provingGround.HoTT.Term with provingGround.HoTT.Subs[provingGround.HoTT
                                                  //| .Term] = (x : (X : Universe(0)))
			val xy = "x->y" :: (X ->: Y)
                                                  //> xy  : provingGround.HoTT.FuncTerm[provingGround.HoTT.Term,provingGround.HoTT
                                                  //| .Term] with provingGround.HoTT.Subs[provingGround.HoTT.FuncTerm[provingGroun
                                                  //| d.HoTT.Term,provingGround.HoTT.Term]] = x->y
	x.typ                                     //> res5: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (X : Universe(0))
	xy.typ                                    //> res6: provingGround.HoTT.Typ[provingGround.HoTT.Term] = ((X : Universe(0)) -
                                                  //| > (Y : Universe(0)))
	xy(x).typ                                 //> res7: provingGround.HoTT.Typ[provingGround.HoTT.Term] = (Y : Universe(0))
	MPall(X)(Y).typ                           //> res8: provingGround.HoTT.Typ[provingGround.HoTT.Term] = ((A : Universe(0)) -
                                                  //| > (((A : Universe(0)) -> (B : Universe(0))) -> (B : Universe(0))))
}