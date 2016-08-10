## Recursion for inductive types

We illustrate construction of inductive types, and defining functions on them recursively.

We begin with some imports. The import Implicits gives the operations to construct inductive types.
```scala
scala> import provingground._
import provingground._

scala> import HoTT._
import HoTT._

scala> import Implicits._
import Implicits._
```

We do not define inductive types, but instead define the _structure of an inductive type_ on a given, typically symbolic type.

The inductive structure is defined using a DSL to specify constructors. The Boolean type has constants true and false as constructors.
Constructors are obtained using the `:::` method on a _Constructor pattern_, which for constants is essentially the inductive type itself.
```scala
scala> val Bool = "Boolean" :: Type
Bool: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Boolean : 𝒰

scala> val BoolInd = "true" ::: Bool |: "false" ::: Bool =: Bool
BoolInd: provingground.ConstructorSeq.Cons[provingground.HoTT.Term,provingground.HoTT.Term] = Cons(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 ),Cons(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 ),Empty(Boolean : 𝒰 )))
```

From the inductive structure, we can obtain the introduction rules.
```scala
scala> val List(tt, ff) = BoolInd.intros
tt: provingground.HoTT.Term = true : (Boolean : 𝒰 )
ff: provingground.HoTT.Term = false : (Boolean : 𝒰 )

scala> tt
res0: provingground.HoTT.Term = true : (Boolean : 𝒰 )

scala> ff
res1: provingground.HoTT.Term = false : (Boolean : 𝒰 )
```

The most important methods on an inductive structure are the `rec` method for making recursive definition on the inductive type,
and the corresponding method for dependent functions. The rec method takes as arguments the data giving the definition for the various constructors.
```scala
scala> BoolInd.rec(Bool)
res2: BoolInd.RecType = (RecSym(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (Boolean : 𝒰 )) ↦ ((RecSym(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (Boolean : 𝒰 )) ↦ (<function1>))

scala> val recBoolBool = BoolInd.rec(Bool)
recBoolBool: BoolInd.RecType = (RecSym(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (Boolean : 𝒰 )) ↦ ((RecSym(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (Boolean : 𝒰 )) ↦ (<function1>))

scala> recBoolBool.typ
res3: provingground.HoTT.Typ[provingground.HoTT.Term] = (Boolean : 𝒰 ) → ((Boolean : 𝒰 ) → ((Boolean : 𝒰 ) → (Boolean : 𝒰 )))
```

The compile time scala type of the recursion function is just `Term`. The `import Fold._` allows pattern matching and using the runtime type.
```scala
scala> import Fold._
import Fold._
```

We can define functions recursively using terms obtained from the `rec` method.
In the case of Booleans, the arguments are just the value of the function at true and false. The result is a function `f: Bool ->: X` for a type `X`
```scala
scala> val not = recBoolBool(ff)(tt)
not: provingground.HoTT.Term = <function1>

scala> not(ff)
res4: provingground.HoTT.Term = true : (Boolean : 𝒰 )

scala> not(tt)
res5: provingground.HoTT.Term = false : (Boolean : 𝒰 )

scala> assert(not(ff) == tt && not(tt) == ff)
```

We can similarly define the _and_ function by observing that _and(true)_ is the identity and _and(false)_ is the constant false function.
```scala
scala> val b= "b" :: Bool
b: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = b : (Boolean : 𝒰 )

scala> val recBBB = BoolInd.rec(Bool ->: Bool)
recBBB: BoolInd.RecType = (RecSym(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 )) : ((Boolean : 𝒰 ) → (Boolean : 𝒰 ))) ↦ ((RecSym(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 )) : ((Boolean : 𝒰 ) → (Boolean : 𝒰 ))) ↦ (<function1>))

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

scala> assert(and(tt)(tt)== tt && and(tt)(ff) == ff && and(ff)(tt) == ff && and(ff)(ff) == ff)
```

The natural numbers `Nat` are an inductive type with two constructors, `zero` and `succ`, of types `Nat` and `Nat ->: Nat`, respectively.
The method on constructors corresponding to function types _with domain the inductive type being specified_ is `-->>:`.
```scala
scala> val Nat ="Nat" :: Type
Nat: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Nat : 𝒰

scala> val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
NatInd: provingground.ConstructorSeq.Cons[provingground.HoTT.Term,provingground.HoTT.Term] = Cons(ConstructorDefn(IdW(),0 : (Nat : 𝒰 ),Nat : 𝒰 ),Cons(ConstructorDefn(FuncPtn(IdIterPtn(),IdW()),succ : ((Nat : 𝒰 ) → (Nat : 𝒰 )),Nat : 𝒰 ),Empty(Nat : 𝒰 )))

scala> val List(zero, succ) = NatInd.intros
zero: provingground.HoTT.Term = 0 : (Nat : 𝒰 )
succ: provingground.HoTT.Term = succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))
```

To define recursively a function `f : Nat ->: X` for a type `X`, the data is

* `f(zero) : X`, i.e., data of type `X`
* `f(succ(n)) : X` as a function of `n : Nat` and `x: X`, i.e., data is of the form `Nat ->: X ->: X`

```scala
scala> val recNatBool = NatInd.rec(Bool)
recNatBool: NatInd.RecType = (RecSym(ConstructorDefn(IdW(),0 : (Nat : 𝒰 ),Nat : 𝒰 )) : (Boolean : 𝒰 )) ↦ ((RecSym(ConstructorDefn(FuncPtn(IdIterPtn(),IdW()),succ : ((Nat : 𝒰 ) → (Nat : 𝒰 )),Nat : 𝒰 )) : ((Nat : 𝒰 ) → ((Boolean : 𝒰 ) → (Boolean : 𝒰 )))) ↦ (<function1>))

scala> recNatBool.typ
res13: provingground.HoTT.Typ[provingground.HoTT.Term] = (Boolean : 𝒰 ) → (((Nat : 𝒰 ) → ((Boolean : 𝒰 ) → (Boolean : 𝒰 ))) → ((Nat : 𝒰 ) → (Boolean : 𝒰 )))

scala> val n = "n" :: Nat
n: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = n : (Nat : 𝒰 )

scala> val isEven = recNatBool(tt)(n :-> (b :-> not(b)))
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
res14: provingground.HoTT.Term = true : (Boolean : 𝒰 )

scala> isEven(three)
res15: provingground.HoTT.Term = false : (Boolean : 𝒰 )
```

A more complicated example is addition of natural numbers.
```scala
scala> val recNNN = NatInd.rec(Nat ->: Nat)
recNNN: NatInd.RecType = (RecSym(ConstructorDefn(IdW(),0 : (Nat : 𝒰 ),Nat : 𝒰 )) : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ↦ ((RecSym(ConstructorDefn(FuncPtn(IdIterPtn(),IdW()),succ : ((Nat : 𝒰 ) → (Nat : 𝒰 )),Nat : 𝒰 )) : ((Nat : 𝒰 ) → (((Nat : 𝒰 ) → (Nat : 𝒰 )) → ((Nat : 𝒰 ) → (Nat : 𝒰 ))))) ↦ (<function1>))

scala> recNNN.typ
res16: provingground.HoTT.Typ[provingground.HoTT.Term] = ((Nat : 𝒰 ) → (Nat : 𝒰 )) → (((Nat : 𝒰 ) → (((Nat : 𝒰 ) → (Nat : 𝒰 )) → ((Nat : 𝒰 ) → (Nat : 𝒰 )))) → ((Nat : 𝒰 ) → ((Nat : 𝒰 ) → (Nat : 𝒰 ))))

scala> val m = "m" :: Nat
m: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = m : (Nat : 𝒰 )

scala> val addn ="add(n)" :: Nat ->: Nat
addn: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = add(n) : ((Nat : 𝒰 ) → (Nat : 𝒰 ))

scala> val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> (succ(addn(m))) ) ) )
add: provingground.HoTT.Term = <function1>

scala> add(two)(one)
res17: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> assert(add(two)(one) == three)

scala> add(two)(two) == four
res19: Boolean = true
```

```scala
scala> val A ="A" :: Type
A: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A : 𝒰

scala> val ListA = "List(A)" :: Type
ListA: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = List(A) : 𝒰

scala> val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
ListAInd: provingground.ConstructorSeq.Cons[provingground.HoTT.Term,provingground.HoTT.Term] = Cons(ConstructorDefn(IdW(),nil : (List(A) : 𝒰 ),List(A) : 𝒰 ),Cons(ConstructorDefn(CnstFncPtn(A : 𝒰 ,FuncPtn(IdIterPtn(),IdW())),cons : ((A : 𝒰 ) → ((List(A) : 𝒰 ) → (List(A) : 𝒰 ))),List(A) : 𝒰 ),Empty(List(A) : 𝒰 )))

scala> val List(nil, cons) = ListAInd.intros
nil: provingground.HoTT.Term = nil : (List(A) : 𝒰 )
cons: provingground.HoTT.Term = cons : ((A : 𝒰 ) → ((List(A) : 𝒰 ) → (List(A) : 𝒰 )))

scala> val recLN = ListAInd.rec(Nat)
recLN: ListAInd.RecType = (RecSym(ConstructorDefn(IdW(),nil : (List(A) : 𝒰 ),List(A) : 𝒰 )) : (Nat : 𝒰 )) ↦ ((RecSym(ConstructorDefn(CnstFncPtn(A : 𝒰 ,FuncPtn(IdIterPtn(),IdW())),cons : ((A : 𝒰 ) → ((List(A) : 𝒰 ) → (List(A) : 𝒰 ))),List(A) : 𝒰 )) : ((A : 𝒰 ) → ((List(A) : 𝒰 ) → ((Nat : 𝒰 ) → (Nat : 𝒰 ))))) ↦ (<function1>))

scala> recLN.typ
res20: provingground.HoTT.Typ[provingground.HoTT.Term] = (Nat : 𝒰 ) → (((A : 𝒰 ) → ((List(A) : 𝒰 ) → ((Nat : 𝒰 ) → (Nat : 𝒰 )))) → ((List(A) : 𝒰 ) → (Nat : 𝒰 )))

scala> val a = "a" :: A
a: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = a : (A : 𝒰 )

scala> val l = "l" :: ListA
l: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = l : (List(A) : 𝒰 )

scala> val n = "n" :: Nat
n: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = n : (Nat : 𝒰 )

scala> val size = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))
size: provingground.HoTT.Term = <function1>

scala> import Fold._
import Fold._

scala> val size = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))
size: provingground.HoTT.Term = <function1>

scala> size(nil)
res21: provingground.HoTT.Term = 0 : (Nat : 𝒰 )

scala> size(cons(a)(cons(a)(nil)))
res22: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> val T ="Tree" :: Type
T: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Tree : 𝒰

scala> val TInd = ("leaf" ::: T) |: ("node" ::: T -->>: T -->>: T) =: T
TInd: provingground.ConstructorSeq.Cons[provingground.HoTT.Term,provingground.HoTT.Term] = Cons(ConstructorDefn(IdW(),leaf : (Tree : 𝒰 ),Tree : 𝒰 ),Cons(ConstructorDefn(FuncPtn(IdIterPtn(),FuncPtn(IdIterPtn(),IdW())),node : ((Tree : 𝒰 ) → ((Tree : 𝒰 ) → (Tree : 𝒰 ))),Tree : 𝒰 ),Empty(Tree : 𝒰 )))

scala> val List(leaf, node) = TInd.intros
leaf: provingground.HoTT.Term = leaf : (Tree : 𝒰 )
node: provingground.HoTT.Term = node : ((Tree : 𝒰 ) → ((Tree : 𝒰 ) → (Tree : 𝒰 )))

scala> import Fold._
import Fold._

scala> val t = node(node(leaf)(node(leaf)(leaf)))(node(leaf)(leaf))
t: provingground.HoTT.Term = ((node : ((Tree : 𝒰 ) → ((Tree : 𝒰 ) → (Tree : 𝒰 )))) (((node : ((Tree : 𝒰 ) → ((Tree : 𝒰 ) → (Tree : 𝒰 )))) (leaf : (Tree : 𝒰 )) : ((Tree : 𝒰 ) → (Tree : 𝒰 ))) (((node : ((Tree : 𝒰 ) → ((Tree : 𝒰 ) → (Tree : 𝒰 )))) (leaf : (Tree : 𝒰 )) : ((Tree : 𝒰 ) → (Tree : 𝒰 ))) (leaf : (Tree : 𝒰 )) : (Tree : 𝒰 )) : (Tree : 𝒰 )) : ((Tree : 𝒰 ) → (Tree : 𝒰 ))) (((node : ((Tree : 𝒰 ) → ((Tree : 𝒰 ) → (Tree : 𝒰 )))) (leaf : (Tree : 𝒰 )) : ((Tree : 𝒰 ) → (Tree : 𝒰 ))) (leaf : (Tree : 𝒰 )) : (Tree : 𝒰 )) : (Tree : 𝒰 )

scala> val recTN = TInd.rec(Nat)
recTN: TInd.RecType = (RecSym(ConstructorDefn(IdW(),leaf : (Tree : 𝒰 ),Tree : 𝒰 )) : (Nat : 𝒰 )) ↦ ((RecSym(ConstructorDefn(FuncPtn(IdIterPtn(),FuncPtn(IdIterPtn(),IdW())),node : ((Tree : 𝒰 ) → ((Tree : 𝒰 ) → (Tree : 𝒰 ))),Tree : 𝒰 )) : ((Tree : 𝒰 ) → ((Nat : 𝒰 ) → ((Tree : 𝒰 ) → ((Nat : 𝒰 ) → (Nat : 𝒰 )))))) ↦ (<function1>))

scala> recTN.typ
res23: provingground.HoTT.Typ[provingground.HoTT.Term] = (Nat : 𝒰 ) → (((Tree : 𝒰 ) → ((Nat : 𝒰 ) → ((Tree : 𝒰 ) → ((Nat : 𝒰 ) → (Nat : 𝒰 ))))) → ((Tree : 𝒰 ) → (Nat : 𝒰 )))

scala> val t1 = "t1" :: T
t1: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = t1 : (Tree : 𝒰 )

scala> val t2 = "t2" :: T
t2: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = t2 : (Tree : 𝒰 )

scala> val vertices = recTN(one)(t1 :-> (m :->( t2 :-> (n :-> (succ(add(n)(m))  ) ) ) ) )
vertices: provingground.HoTT.Term = <function1>

scala> vertices(t)
res24: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> val nine = succ(add(four)(four))
nine: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> vertices(t) == nine
res25: Boolean = true

scala> val BT ="BinTree" :: Type
BT: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = BinTree : 𝒰

scala> val BTInd = ("leaf" ::: BT) |: ("node" ::: (Bool -|>: BT) -->>: BT )  =: BT
BTInd: provingground.ConstructorSeq.Cons[provingground.HoTT.Term,provingground.HoTT.Term] = Cons(ConstructorDefn(IdW(),leaf : (BinTree : 𝒰 ),BinTree : 𝒰 ),Cons(ConstructorDefn(FuncPtn(FuncIterPtn(Boolean : 𝒰 ,IdIterPtn()),IdW()),node : (((Boolean : 𝒰 ) → (BinTree : 𝒰 )) → (BinTree : 𝒰 )),BinTree : 𝒰 ),Empty(BinTree : 𝒰 )))

scala> val List(leaf, node) = BTInd.intros
leaf: provingground.HoTT.Term = leaf : (BinTree : 𝒰 )
node: provingground.HoTT.Term = node : (((Boolean : 𝒰 ) → (BinTree : 𝒰 )) → (BinTree : 𝒰 ))

scala> val recBTN = BTInd.rec(Nat)
recBTN: BTInd.RecType = (RecSym(ConstructorDefn(IdW(),leaf : (BinTree : 𝒰 ),BinTree : 𝒰 )) : (Nat : 𝒰 )) ↦ ((RecSym(ConstructorDefn(FuncPtn(FuncIterPtn(Boolean : 𝒰 ,IdIterPtn()),IdW()),node : (((Boolean : 𝒰 ) → (BinTree : 𝒰 )) → (BinTree : 𝒰 )),BinTree : 𝒰 )) : (((Boolean : 𝒰 ) → (BinTree : 𝒰 )) → (((Boolean : 𝒰 ) → (Nat : 𝒰 )) → (Nat : 𝒰 )))) ↦ (<function1>))

scala> recBTN.typ
res26: provingground.HoTT.Typ[provingground.HoTT.Term] = (Nat : 𝒰 ) → ((((Boolean : 𝒰 ) → (BinTree : 𝒰 )) → (((Boolean : 𝒰 ) → (Nat : 𝒰 )) → (Nat : 𝒰 ))) → ((BinTree : 𝒰 ) → (Nat : 𝒰 )))

scala> val f = "f" :: Bool ->: BT
f: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = f : ((Boolean : 𝒰 ) → (BinTree : 𝒰 ))

scala> val g = "g" :: Bool ->: Nat
g: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = g : ((Boolean : 𝒰 ) → (Nat : 𝒰 ))

scala> val leaves = recBTN(one)(f :-> (g :-> (add(g(ff))(g(tt))) ))
leaves: provingground.HoTT.Term = <function1>

scala> leaves(leaf)
res27: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> val b = "b" :: Bool
b: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = b : (Boolean : 𝒰 )

scala> val t = node(b :-> leaf)
t: provingground.HoTT.Term = (node : (((Boolean : 𝒰 ) → (BinTree : 𝒰 )) → (BinTree : 𝒰 ))) ((b : (Boolean : 𝒰 )) ↦ (leaf : (BinTree : 𝒰 ))) : (BinTree : 𝒰 )

scala> val recBBT = BoolInd.rec(BT)
recBBT: BoolInd.RecType = (RecSym(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (BinTree : 𝒰 )) ↦ ((RecSym(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (BinTree : 𝒰 )) ↦ (<function1>))

scala> recBBT.typ
res28: provingground.HoTT.Typ[provingground.HoTT.Term] = (BinTree : 𝒰 ) → ((BinTree : 𝒰 ) → ((Boolean : 𝒰 ) → (BinTree : 𝒰 )))

scala> val ttn = recBBT(leaf)(t)
ttn: provingground.HoTT.Term = <function1>

scala> val t2 = node(ttn)
t2: provingground.HoTT.Term = (node : (((Boolean : 𝒰 ) → (BinTree : 𝒰 )) → (BinTree : 𝒰 ))) (<function1>) : (BinTree : 𝒰 )

scala> leaves(t2)
res29: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )
```
