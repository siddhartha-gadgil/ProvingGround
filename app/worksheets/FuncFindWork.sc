package worksheets

import provingground.IntTypes._

import provingground.HoTT._

object FuncFindWork {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import dsl._
  
  val z = i[Long](Z)                              //> z  : provingground.IntTypes.SimpleRep[Long] = SimpleRep(Z)
  
  val zz = z -->: z                               //> zz  : provingground.IntTypes.FuncRep[provingground.HoTT.Term,Long,provinggro
                                                  //| und.HoTT.Term,Long] = FuncRep(SimpleRep(Z),SimpleRep(Z))
  val double = (n: Long) => n *2                  //> double  : Long => Long = <function1>
  
   val d = zz(double)                             //> d  : provingground.IntTypes.ExtendedFunction[provingground.HoTT.Term,Long,pr
                                                  //| ovingground.HoTT.Term,Long] = <function1>
   d(z(3))                                        //> res0: provingground.HoTT.Term = 6
   
   z.unapply(z(3))                                //> res1: Option[Long] = Some(3)
   
   val n = i[Int](N)                              //> n  : provingground.IntTypes.SimpleRep[Int] = SimpleRep(N)
   
   def f(k: Int) = i[Int](FinTyp(n(k))) -->: n    //> f: (k: Int)provingground.IntTypes.FuncRep[provingground.HoTT.Term,Int,provin
                                                  //| gground.HoTT.Term,Int]
  
   
   n ++ f                                         //> res2: provingground.IntTypes.SigmaRep[provingground.HoTT.Term,Int,provinggro
                                                  //| und.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT.Term],Int => In
                                                  //| t] = SigmaRep(SimpleRep(N),<function1>)
   
   def fin(k: Int) = i[Int](FinTyp(n(k)))         //> fin: (k: Int)provingground.IntTypes.SimpleRep[Int]
   
   val r= (s(n)((k: Int) => fin(k) -->: n)) -->: n//> r  : provingground.IntTypes.FuncRep[provingground.HoTT.Term,(Int, Int => Int
                                                  //| ),provingground.HoTT.Term,Int] = FuncRep(SigmaRep(SimpleRep(N),<function1>),
                                                  //| SimpleRep(N))
   r.apply _                                      //> res3: (((Int, Int => Int)) => Int) => provingground.IntTypes.ExtendedFunctio
                                                  //| n[provingground.HoTT.Term,(Int, Int => Int),provingground.HoTT.Term,Int] = <
                                                  //| function1>
   
   val sty = s(n)((k: Int) => fin(k) -->: n)      //> sty  : provingground.IntTypes.SigmaRep[provingground.HoTT.Term,Int,provinggr
                                                  //| ound.HoTT.FuncTerm[provingground.HoTT.Term,provingground.HoTT.Term],Int => I
                                                  //| nt] = SigmaRep(SimpleRep(N),<function1>)
   
   sty.fibers                                     //> res4: provingground.IntTypes.SimpleExtendedFunction[provingground.HoTT.Term,
                                                  //| Int,provingground.HoTT.Typ[provingground.HoTT.Term]] = <function1>
   
   sty.fibers(n(1))                               //> res5: provingground.HoTT.Typ[provingground.HoTT.Term] = (FinTyp(1)⟶N)
   
   
   sty.typ                                        //> res6: provingground.HoTT.SigmaTyp[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term] = SigmaTyp(<function1>)
   SigmaTyp(sty.fibers)                           //> res7: provingground.HoTT.SigmaTyp[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term] = SigmaTyp(<function1>)
   sty.typ.fibers                                 //> res8: provingground.HoTT.TypFamily[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Term] = <function1>
   
   def sum(kf: (Int, Int => Int)) = ((0 to (kf._1 -1)) map (kf._2)).sum
                                                  //> sum: (kf: (Int, Int => Int))Int
   
   r(sum)                                         //> res9: provingground.IntTypes.ExtendedFunction[provingground.HoTT.Term,(Int, 
                                                  //| Int => Int),provingground.HoTT.Term,Int] = <function1>
   r(sum).typ                                     //> res10: provingground.HoTT.FuncTyp[provingground.HoTT.Term,provingground.HoTT
                                                  //| .Term] = (SigmaTyp(<function1>)⟶N)
      }