package worksheets

import provingground.functionfinder.IntTypes._

import provingground.functionfinder.ScalaRep._

import provingground.HoTT._

object FuncFindWork {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import dsl._
  
  val z = i[Long](Z)                              //> z  : provingground.functionfinder.ScalaRep.SimpleRep[Long] = SimpleRep(Z)
  
  val zz = z -->: z                               //> zz  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Term,
                                                  //| Long,provingground.HoTT.Term,Long] = FuncRep(SimpleRep(Z),SimpleRep(Z))
  val double = (n: Long) => n *2                  //> double  : Long => Long = <function1>
  
   val d = zz(double)                             //> d  : provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term] = <function1>
   d(z(3))                                        //> res0: provingground.HoTT.Term = 6
   
   z.unapply(z(3))                                //> res1: Option[Long] = Some(3)
   
   val n = i[Int](N)                              //> n  : provingground.functionfinder.ScalaRep.SimpleRep[Int] = SimpleRep(N)
   
   def f(k: Int) = i[Int](FinTyp(n(k))) -->: n    //> f: (k: Int)provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.
                                                  //| Term,Int,provingground.HoTT.Term,Int]
  
   
   n ++ f                                         //> res2: provingground.functionfinder.ScalaRep.SigmaRep[provingground.HoTT.Term
                                                  //| ,Int,provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT.
                                                  //| Term],Int => Int] = SigmaRep(SimpleRep(N),<function1>)
   
   def fin(k: Int) = i[Int](FinTyp(n(k)))         //> fin: (k: Int)provingground.functionfinder.ScalaRep.SimpleRep[Int]
   
   val r= (s(n)((k: Int) => fin(k) -->: n)) -->: n//> r  : provingground.functionfinder.ScalaRep.FuncRep[provingground.HoTT.Term,(
                                                  //| Int, Int => Int),provingground.HoTT.Term,Int] = FuncRep(SigmaRep(SimpleRep(N
                                                  //| ),<function1>),SimpleRep(N))
   r.apply _                                      //> res3: (((Int, Int => Int)) => Int) => provingground.HoTT.FuncTerm[provinggro
                                                  //| und.HoTT.Term,provingground.HoTT.Term] = <function1>
   
   val sty = s(n)((k: Int) => fin(k) -->: n)      //> sty  : provingground.functionfinder.ScalaRep.SigmaRep[provingground.HoTT.Ter
                                                  //| m,Int,provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term],Int => Int] = SigmaRep(SimpleRep(N),<function1>)
   
   sty.fibers                                     //> res4: provingground.functionfinder.ScalaRep.SimpleExtendedFunction[provinggr
                                                  //| ound.HoTT.Term,Int,provingground.HoTT.Typ[provingground.HoTT.Term]] = <funct
                                                  //| ion1>
   
   sty.fibers(n(1))                               //> res5: provingground.HoTT.Typ[provingground.HoTT.Term] = (FinTyp(1)⟶N)
   
   
   sty.typ                                        //> res6: provingground.HoTT.SigmaTyp[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term] = SigmaTyp(<function1>)
   SigmaTyp(sty.fibers)                           //> res7: provingground.HoTT.SigmaTyp[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term] = SigmaTyp(<function1>)
   sty.typ.fibers                                 //> res8: provingground.HoTT.TypFamily[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = <function1>
   
   def sum(kf: (Int, Int => Int)) = ((0 to (kf._1 -1)) map (kf._2)).sum
                                                  //> sum: (kf: (Int, Int => Int))Int
   
   r(sum)                                         //> res9: provingground.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term] = <function1>
   r(sum).typ                                     //> res10: provingground.HoTT.Typ[provingground.HoTT.Term] = (SigmaTyp(<function
                                                  //| 1>)⟶N)
      }