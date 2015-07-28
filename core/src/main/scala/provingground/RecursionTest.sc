package provingground

import HoTT._
import org.scalatest.FlatSpec

import ConstructorPattern._

import ConstructorPattern._

import RecFunction._

object RecursionTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  case object Bool extends SmallTyp
  
  case object Nat extends SmallTyp

  val ttC  = W.constructor(Bool, "true")          //> ttC  : provingground.Constructor[provingground.HoTT.Term,provingground.Const
                                                  //| ructorPattern.W.ConstructorType] = ConstructorDefn(IdW,true : (Bool),Bool)

  val ffC = W.constructor(Bool, "false")          //> ffC  : provingground.Constructor[provingground.HoTT.Term,provingground.Const
                                                  //| ructorPattern.W.ConstructorType] = ConstructorDefn(IdW,false : (Bool),Bool)

  val tt : Term = ttC.cons                        //> tt  : provingground.HoTT.Term = true : (Bool)

  val ff : Term = ffC.cons                        //> ff  : provingground.HoTT.Term = false : (Bool)

  val BoolCons = List(ttC, ffC)                   //> BoolCons  : List[provingground.Constructor[provingground.HoTT.Term,provinggr
                                                  //| ound.ConstructorPattern.W.ConstructorType]] = List(ConstructorDefn(IdW,true 
                                                  //| : (Bool),Bool), ConstructorDefn(IdW,false : (Bool),Bool))
  
  val recBool = recFunction(BoolCons, Bool)       //> recBool  : provingground.RecFunction[provingground.HoTT.Term] = RecFunctionC
                                                  //| ons(<function1>,<function1>,RecFunctionCons(<function1>,<function1>,RecTail(
                                                  //| Bool)))
  val dummy = recBool.fullTyp(Bool).symbObj("dummy-function")
                                                  //> dummy  : provingground.RecursionTest.recBool.FullType with provingground.HoT
                                                  //| T.Subs[provingground.RecursionTest.recBool.FullType] = dummy-function : ((Bo
                                                  //| ol) → ((Bool) → ((Bool) → (Bool))))
  
  val dummyFn = dummy.asInstanceOf[Func[Term, Func[Term, Func[Term, Term]]]]
                                                  //> dummyFn  : provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoT
                                                  //| T.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Te
                                                  //| rm,provingground.HoTT.Term]]] = dummy-function : ((Bool) → ((Bool) → ((B
                                                  //| ool) → (Bool))))
  
  dummyFn(tt)                                     //> res0: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Fun
                                                  //| c[provingground.HoTT.Term,provingground.HoTT.Term]] = (dummy-function : ((Bo
                                                  //| ol) → ((Bool) → ((Bool) → (Bool))))) (true : (Bool)) : ((Bool) → ((B
                                                  //| ool) → (Bool)))
  
  dummyFn(tt)(ff)                                 //> res1: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Ter
                                                  //| m] = ((dummy-function : ((Bool) → ((Bool) → ((Bool) → (Bool))))) (true
                                                  //|  : (Bool)) : ((Bool) → ((Bool) → (Bool)))) (false : (Bool)) : ((Bool) �
                                                  //| � (Bool))
  
  val boolBoolFn = recBool.recursion(Bool)(dummy).asInstanceOf[Func[Term, Func[Term, Func[Term, Term]]]]
                                                  //> boolBoolFn  : provingground.HoTT.Func[provingground.HoTT.Term,provingground.
                                                  //| HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT
                                                  //| .Term,provingground.HoTT.Term]]] = (a : (Bool)) ↦ ((c : (Bool)) ↦ ((d : 
                                                  //| (Bool)) ↦ ((((dummy-function : ((Bool) → ((Bool) → ((Bool) → (Bool))
                                                  //| ))) (a : (Bool)) : ((Bool) → ((Bool) → (Bool)))) (b : (Bool)) : ((Bool) 
                                                  //| → (Bool))) (d : (Bool)) : (Bool))))
  
  val neg = boolBoolFn(ff)(tt)                    //> neg  : provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Te
                                                  //| rm] = (d : (Bool)) ↦ ((((dummy-function : ((Bool) → ((Bool) → ((Bool) 
                                                  //| → (Bool))))) (a : (Bool)) : ((Bool) → ((Bool) → (Bool)))) (e : (Bool))
                                                  //|  : ((Bool) → (Bool))) (d : (Bool)) : (Bool))
  
  neg                                             //> res2: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Ter
                                                  //| m] = (d : (Bool)) ↦ ((((dummy-function : ((Bool) → ((Bool) → ((Bool) �
                                                  //| �� (Bool))))) (a : (Bool)) : ((Bool) → ((Bool) → (Bool)))) (e : (Bool)) 
                                                  //| : ((Bool) → (Bool))) (d : (Bool)) : (Bool))
  
  val id = boolBoolFn(tt)(ff)                     //> id  : provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Ter
                                                  //| m] = (d : (Bool)) ↦ ((((dummy-function : ((Bool) → ((Bool) → ((Bool) �
                                                  //| �� (Bool))))) (a : (Bool)) : ((Bool) → ((Bool) → (Bool)))) (g : (Bool)) 
                                                  //| : ((Bool) → (Bool))) (d : (Bool)) : (Bool))
  
  neg == id                                       //> res3: Boolean = false
  
  neg(ff)                                         //> res4: provingground.HoTT.Term = a : (Bool)
  
  neg(tt)                                         //> res5: provingground.HoTT.Term = e : (Bool)
  
  neg("nothing" :: Bool)                          //> res6: provingground.HoTT.Term = (((dummy-function : ((Bool) → ((Bool) → 
                                                  //| ((Bool) → (Bool))))) (a : (Bool)) : ((Bool) → ((Bool) → (Bool)))) (e :
                                                  //|  (Bool)) : ((Bool) → (Bool))) (nothing : (Bool)) : (Bool)
  
  val recBoolNat = recBool.recursion(Nat)(recBool.fullTyp(Nat).symbObj("dummy-function"))
                                                  //> recBoolNat  : provingground.RecursionTest.recBool.FullType = (i : (Nat)) �
                                                  //| � ((k : (Nat)) ↦ ((d : (Bool)) ↦ ((((dummy-function : ((Nat) → ((Nat)
                                                  //|  → ((Bool) → (Nat))))) (i : (Nat)) : ((Nat) → ((Bool) → (Nat)))) (j
                                                  //|  : (Nat)) : ((Bool) → (Nat))) (d : (Bool)) : (Nat))))
	recBoolNat.typ                            //> res7: provingground.HoTT.Typ[provingground.HoTT.Term] = ∏((i : (Nat)) ↦
                                                  //|  ((Nat) → ((Bool) → (Nat))))
}