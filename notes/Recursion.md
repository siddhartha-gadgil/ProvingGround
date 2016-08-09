We look at the basic recursion

```scala
scala> import provingground._
import provingground._

scala> import HoTT._
import HoTT._

scala> val A ="A" :: Type
A: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A : 𝒰

scala> import Implicits._
import Implicits._

scala> val Bool = "Boolean" :: Type
Bool: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Boolean : 𝒰

scala> val boolInduc = "true" ::: Bool |: "false" ::: Bool =: Bool
boolInduc: provingground.ConstructorSeq.Cons[provingground.HoTT.Term,provingground.HoTT.Term] = Cons(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 ),Cons(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 ),Empty(Boolean : 𝒰 )))

scala> val List(tt, ff) = boolInduc.intros
tt: provingground.HoTT.Term = true : (Boolean : 𝒰 )
ff: provingground.HoTT.Term = false : (Boolean : 𝒰 )

scala> tt
res0: provingground.HoTT.Term = true : (Boolean : 𝒰 )

scala> ff
res1: provingground.HoTT.Term = false : (Boolean : 𝒰 )

scala> boolInduc.rec(Bool)
res2: boolInduc.RecType = (RecSym(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (Boolean : 𝒰 )) ↦ ((RecSym(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (Boolean : 𝒰 )) ↦ (<function1>))

scala> val recBoolBool = boolInduc.rec(Bool)
recBoolBool: boolInduc.RecType = (RecSym(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (Boolean : 𝒰 )) ↦ ((RecSym(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (Boolean : 𝒰 )) ↦ (<function1>))

scala> recBoolBool.typ
res3: provingground.HoTT.Typ[provingground.HoTT.Term] = (Boolean : 𝒰 ) → ((Boolean : 𝒰 ) → ((Boolean : 𝒰 ) → (Boolean : 𝒰 )))

scala> import Fold._
import Fold._

scala> val not = recBoolBool(ff)(tt)
not: provingground.HoTT.Term = <function1>

scala> not(ff)
res4: provingground.HoTT.Term = true : (Boolean : 𝒰 )

scala> not(tt)
res5: provingground.HoTT.Term = false : (Boolean : 𝒰 )

scala> boolInduc
res6: provingground.ConstructorSeq.Cons[provingground.HoTT.Term,provingground.HoTT.Term] = Cons(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 ),Cons(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 ),Empty(Boolean : 𝒰 )))

scala> val b= "b" :: Bool
b: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = b : (Boolean : 𝒰 )

scala> val recBBB = boolInduc.rec(Bool ->: Bool)
recBBB: boolInduc.RecType = (RecSym(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 )) : ((Boolean : 𝒰 ) → (Boolean : 𝒰 ))) ↦ ((RecSym(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 )) : ((Boolean : 𝒰 ) → (Boolean : 𝒰 ))) ↦ (<function1>))

scala> recBBB.typ
res7: provingground.HoTT.Typ[provingground.HoTT.Term] = ((Boolean : 𝒰 ) → (Boolean : 𝒰 )) → (((Boolean : 𝒰 ) → (Boolean : 𝒰 )) → ((Boolean : 𝒰 ) → ((Boolean : 𝒰 ) → (Boolean : 𝒰 ))))

scala> val and = recBBB(lmbda(b)(b))(lmbda(b)(ff))
and: provingground.HoTT.Term = <function1>

scala> and(tt)(tt)
res8: provingground.HoTT.Term = true : (Boolean : 𝒰 )

scala> and(tt)(ff)
res9: provingground.HoTT.Term = false : (Boolean : 𝒰 )

scala> and(ff)(ff)
res10: provingground.HoTT.Term = false : (Boolean : 𝒰 )

scala> and(ff)(tt)
res11: provingground.HoTT.Term = false : (Boolean : 𝒰 )

scala> val Nat ="Nat" :: Type
Nat: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Nat : 𝒰

scala> val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
NatInd: provingground.ConstructorSeq.Cons[provingground.HoTT.Term,provingground.HoTT.Term] = Cons(ConstructorDefn(IdW(),0 : (Nat : 𝒰 ),Nat : 𝒰 ),Cons(ConstructorDefn(FuncPtn(IdIterPtn(),IdW()),succ : ((Nat : 𝒰 ) → (Nat : 𝒰 )),Nat : 𝒰 ),Empty(Nat : 𝒰 )))

scala> val List(zero, succ) = NatInd.intros
zero: provingground.HoTT.Term = 0 : (Nat : 𝒰 )
succ: provingground.HoTT.Term = succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))

scala> val recNatBool = NatInd.rec(Bool)
recNatBool: NatInd.RecType = (RecSym(ConstructorDefn(IdW(),0 : (Nat : 𝒰 ),Nat : 𝒰 )) : (Boolean : 𝒰 )) ↦ ((RecSym(ConstructorDefn(FuncPtn(IdIterPtn(),IdW()),succ : ((Nat : 𝒰 ) → (Nat : 𝒰 )),Nat : 𝒰 )) : ((Nat : 𝒰 ) → ((Boolean : 𝒰 ) → (Boolean : 𝒰 )))) ↦ (<function1>))

scala> recNatBool.typ
res12: provingground.HoTT.Typ[provingground.HoTT.Term] = (Boolean : 𝒰 ) → (((Nat : 𝒰 ) → ((Boolean : 𝒰 ) → (Boolean : 𝒰 ))) → ((Nat : 𝒰 ) → (Boolean : 𝒰 )))

scala> val n = "n" :: Nat
n: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = n : (Nat : 𝒰 )

scala> val isEven = recNatBool(tt)(lmbda(n)(lmbda(b)(not(b))))
isEven: provingground.HoTT.Term = <function1>

scala> val one = succ(zero)
one: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> val two = succ(one)
two: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> val three = succ(two)
three: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> val four = succ(three)
four: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> isEven(two)
res13: provingground.HoTT.Term = true : (Boolean : 𝒰 )

scala> isEven(three)
res14: provingground.HoTT.Term = false : (Boolean : 𝒰 )

scala> val recNNN = NatInd.rec(Nat ->: Nat)
recNNN: NatInd.RecType = (RecSym(ConstructorDefn(IdW(),0 : (Nat : 𝒰 ),Nat : 𝒰 )) : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ↦ ((RecSym(ConstructorDefn(FuncPtn(IdIterPtn(),IdW()),succ : ((Nat : 𝒰 ) → (Nat : 𝒰 )),Nat : 𝒰 )) : ((Nat : 𝒰 ) → (((Nat : 𝒰 ) → (Nat : 𝒰 )) → ((Nat : 𝒰 ) → (Nat : 𝒰 ))))) ↦ (<function1>))

scala> recNNN.typ
res15: provingground.HoTT.Typ[provingground.HoTT.Term] = ((Nat : 𝒰 ) → (Nat : 𝒰 )) → (((Nat : 𝒰 ) → (((Nat : 𝒰 ) → (Nat : 𝒰 )) → ((Nat : 𝒰 ) → (Nat : 𝒰 )))) → ((Nat : 𝒰 ) → ((Nat : 𝒰 ) → (Nat : 𝒰 ))))

scala> val m = "m" :: Nat
m: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = m : (Nat : 𝒰 )

scala> val addn ="add(n)" :: Nat ->: Nat
addn: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = add(n) : ((Nat : 𝒰 ) → (Nat : 𝒰 ))

scala> val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> (succ(addn(m))) ) ) )
add: provingground.HoTT.Term = <function1>

scala> add(two)(one)
res16: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> assert(add(two)(one) == three)

scala> add(two)(two) == four
res18: Boolean = true
```
