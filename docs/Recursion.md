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
The method on constructors corresponding to function types we use if `-->>:`, which is used because the domain of the extension is also the type `Nat`. Note that extending the constructor by a constant type is very different (as we see with lists below), and a different method is used.
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

Lists of elements of a type `A` form an inductive type `ListA`, again with two constructors:

* `nil` of type `ListA`
* `cons` of type `A ->: ListA ->: ListA`

A recursively defined function `f` to a type `X` is specified by data:

* `f(nil) : X`
* `f(cons(a)(l))` as a function of `a`, `l` and 'f(l)', i.e., data has type `A ->: ListA ->: X ->: X`.

Note that `f(a)` does not make sense. Hence a different method, `->>:`, is used for such extensions.

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
```

We can define the size of a list as a natural number recursively.
```scala
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

scala> size(nil)
res21: provingground.HoTT.Term = 0 : (Nat : 𝒰 )

scala> size(cons(a)(cons(a)(nil)))
res22: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )
```

Another interesting inductive type is a binary rooted tree. This is our first description.
We define the number of vertices recursively on this.
```scala
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

scala> assert(vertices(t) == nine)
```

We can implement binary trees in another way, which generalizes to binary rooted trees with varying degree.
Instead of a pair of trees, a node corresponds to functions from Booleans to binary rooted trees.

This involves more complex constructors, with an additional method `-|>:`.
The data for recursively defining `f` is also more complex.
We define the number of leaves in such a tree recursively.
```scala
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
res27: provingground.HoTT.Typ[provingground.HoTT.Term] = (Nat : 𝒰 ) → ((((Boolean : 𝒰 ) → (BinTree : 𝒰 )) → (((Boolean : 𝒰 ) → (Nat : 𝒰 )) → (Nat : 𝒰 ))) → ((BinTree : 𝒰 ) → (Nat : 𝒰 )))

scala> val f = "f" :: Bool ->: BT
f: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = f : ((Boolean : 𝒰 ) → (BinTree : 𝒰 ))

scala> val g = "g" :: Bool ->: Nat
g: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = g : ((Boolean : 𝒰 ) → (Nat : 𝒰 ))

scala> val leaves = recBTN(one)(f :-> (g :-> (add(g(ff))(g(tt))) ))
leaves: provingground.HoTT.Term = <function1>

scala> leaves(leaf)
res28: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> val b = "b" :: Bool
b: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = b : (Boolean : 𝒰 )

scala> val t = node(b :-> leaf)
t: provingground.HoTT.Term = (node : (((Boolean : 𝒰 ) → (BinTree : 𝒰 )) → (BinTree : 𝒰 ))) ((b : (Boolean : 𝒰 )) ↦ (leaf : (BinTree : 𝒰 ))) : (BinTree : 𝒰 )

scala> val recBBT = BoolInd.rec(BT)
recBBT: BoolInd.RecType = (RecSym(ConstructorDefn(IdW(),true : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (BinTree : 𝒰 )) ↦ ((RecSym(ConstructorDefn(IdW(),false : (Boolean : 𝒰 ),Boolean : 𝒰 )) : (BinTree : 𝒰 )) ↦ (<function1>))

scala> recBBT.typ
res29: provingground.HoTT.Typ[provingground.HoTT.Term] = (BinTree : 𝒰 ) → ((BinTree : 𝒰 ) → ((Boolean : 𝒰 ) → (BinTree : 𝒰 )))

scala> val ttn = recBBT(leaf)(t)
ttn: provingground.HoTT.Term = <function1>

scala> val t2 = node(ttn)
t2: provingground.HoTT.Term = (node : (((Boolean : 𝒰 ) → (BinTree : 𝒰 )) → (BinTree : 𝒰 ))) (<function1>) : (BinTree : 𝒰 )

scala> leaves(t2)
res30: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )
```

As some expresssions are very long, we import a method "FansiShow" that prints in a more concise way.
In the REPL, this gives coloured output using ANSI strings.
```scala
scala> import FansiShow._
import FansiShow._
```

We define the double of a number recursively, mainly for use later. Observe the partial simplification.
```scala
scala> val recNN = NatInd.rec(Nat)
recNN: NatInd.RecType = (RecSym(ConstructorDefn(IdW(),0 : (Nat : 𝒰 ),Nat : 𝒰 )) : (Nat : 𝒰 )) ↦ ((RecSym(ConstructorDefn(FuncPtn(IdIterPtn(),IdW()),succ : ((Nat : 𝒰 ) → (Nat : 𝒰 )),Nat : 𝒰 )) : ((Nat : 𝒰 ) → ((Nat : 𝒰 ) → (Nat : 𝒰 )))) ↦ (<function1>))

scala> val double = recNN(zero)(m :-> (n :-> (succ(succ(n)))))
double: provingground.HoTT.Term = <function1>

scala> double(two) == four
res31: Boolean = true

scala> assert(double(two) == four)

scala> double(succ(n))
res33: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((<function1>) (n : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )
```

All our recursive definitions so far of functions `f` have ignored `n` in defining `f(succ(n))`,
and are only in terms of `f(n)`. We see a more complex definition, the sum of numbers up to `n`.
Note that we are defining `sumTo(succ(m))` in terms of `m` and `n = sumTo(m)`, so this is `add(succ(m))(n)`
```scala
scala> val sumTo = recNN(zero)(m :-> (n :-> (add(succ(m))(n))))
sumTo: provingground.HoTT.Term = <function1>

scala> sumTo(one)
res34: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> sumTo(three).fansi
res35: String = succ(succ(succ(succ(succ(succ(0))))))

scala> val ten = succ(nine)
ten: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> sumTo(four) == ten
res36: Boolean = true

scala> assert(sumTo(four) == ten)
```



## Inductive definitions

In homotopy type theory, inductive definitions are the analogues of recursive definitions for dependent functions.
We see an example of such a definition.

The image is a family `V : Nat ->: Type` which we can think of as vectors of natural numbers indexed by length.
Just like actual vectors, we have `nil` and `cons` introduction rules, but here they are purely formal.

```scala
scala> val V = "Vec" :: Nat ->: Type
V: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = Vec : ((Nat : 𝒰 ) → (𝒰 _0))

scala> val nilv = "nil" :: V(zero)
nilv: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = nil : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 )

scala> val consv = "cons" :: n ~>: (Nat ->: V(n) ->: V(succ(n)))
consv: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] with provingground.HoTT.Subs[provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = cons : (∏((n : (Nat : 𝒰 )) ↦ ((Nat : 𝒰 ) → (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (n : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (n : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))))
```

We have an induction function taking data for the cases and returning a dependent function.
This is defined by giving data for cases corresponding to the constructors.
Namely to define the dependent function `f`, we must specify

* `f(zero)` of type `V(zero)`
* `f(succ(m))` of type `V(succ(m))`, as a dependent function of `m` and of `f(m) : V(m)`.


We define inductively a countdown function, giving the vector counting down from `n`.
```scala
scala> val indNV = NatInd.induc(V)
indNV: NatInd.InducType = (InducSym(ConstructorDefn(IdW(),0 : (Nat : 𝒰 ),Nat : 𝒰 )) : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 )) ↦ ((InducSym(ConstructorDefn(FuncPtn(IdIterPtn(),IdW()),succ : ((Nat : 𝒰 ) → (Nat : 𝒰 )),Nat : 𝒰 )) : (∏(($m : (Nat : 𝒰 )) ↦ (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ($m : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ($m : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))))) ↦ (<function1>))

scala> val v = "v_m" :: V(m)
v: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = v_m : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (m : (Nat : 𝒰 )) : 𝒰 )

scala> val countdown = indNV(nilv)(m :~> (v :-> consv(m)(succ(m))(v)) )
countdown: provingground.HoTT.Term = <function1>

scala> countdown(zero)
res38: provingground.HoTT.Term = nil : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 )

scala> countdown(one)
res39: provingground.HoTT.Term = (((cons : (∏((n : (Nat : 𝒰 )) ↦ ((Nat : 𝒰 ) → (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (n : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (n : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))))) (0 : (Nat : 𝒰 )) : ((Nat : 𝒰 ) → (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))) (nil : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 )) : ((Vec : ((Nat ...

scala> countdown(one).fansi
res40: String = cons(0)(succ(0))(nil)

scala> countdown(three).fansi
res41: String = cons(succ(succ(0)))(succ(succ(succ(0))))(cons(succ(0))(succ(succ(0)))(cons(0)(succ(0))(nil)))

scala> assert(countdown(three) ==
     |   consv(two)(three)(
     |     consv(one)(two)(
     |       consv(zero)(one)(nilv))))

scala> countdown(zero) == nilv
res43: Boolean = true

scala> countdown(nine).fansi
res44: String = cons(succ(succ(succ(succ(succ(succ(succ(succ(0)))))))))(succ(succ(succ(succ(succ(succ(succ(succ(succ(0))))))))))(cons(succ(succ(succ(succ(succ(succ(succ(0))))))))(succ(succ(succ(succ(succ(succ(succ(succ(0)))))))))(cons(succ(succ(succ(succ(succ(succ(0)))))))(succ(succ(succ(succ([3...
```

We now illustrate a simple instance of using _propositions as proofs_.
The type family `isEven : Nat ->: Type` gives a type representing whether a natural number is even.
This is an inductive type, but here we simply specify the type by  its introduction rules (constructors).
Such terms introduced by specifying types are logically _axioms_.

```scala
scala> val isEven = "isEven" :: Nat ->: Type
isEven: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = isEven : ((Nat : 𝒰 ) → (𝒰 _0))

scala> val zeroEven = "0even" :: isEven(zero)
zeroEven: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = 0even : ((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 )

scala> val plusTwoEven = "_+2even" :: (n ~>: (isEven(n) ->: isEven(succ(succ(n)))))
plusTwoEven: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = _+2even : (∏((n : (Nat : 𝒰 )) ↦ (((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) (n : (Nat : 𝒰 )) : 𝒰 ) → ((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (n : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))))
```

One can directly see that two and four are even.
```scala
scala> val TwoEven = plusTwoEven(zero)(zeroEven)  !: isEven(two)
TwoEven: provingground.HoTT.Term = ((_+2even : (∏((n : (Nat : 𝒰 )) ↦ (((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) (n : (Nat : 𝒰 )) : 𝒰 ) → ((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (n : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))))) (0 : (Nat : 𝒰 )) : (((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ) → ((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))) (0even : ((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 )) : ((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )

scala> val FourEven = plusTwoEven(two)(TwoEven) !: isEven(four)
FourEven: provingground.HoTT.Term = ((_+2even : (∏((n : (Nat : 𝒰 )) ↦ (((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) (n : (Nat : 𝒰 )) : 𝒰 ) → ((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (n : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ) → ((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (N...
```

Here is a simple proof by induction. We prove the statement that the _double_ of every natural number is even.
The `induc` method gives a dependent function, which takes the base case and the induction step as arguments.
The _base case_ is inhabited by the constructor of type `isEven(zero)`.
The _induction step_ for `n` is a term of type `isEven(double(succ(n)))` as a function of `n` and
the _induction hypothesis_. Note that the induction hypothesis is a term of type `isEven(double(n))`.
```scala
scala> val thmDoubleEven = n ~>: isEven(double(n))
thmDoubleEven: provingground.HoTT.PiTyp[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = ∏((n : (Nat : 𝒰 )) ↦ ((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) ((<function1>) (n : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))

scala> val hyp = "isEven(double(n))" :: isEven(double(n))
hyp: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = isEven(double(n)) : ((isEven : ((Nat : 𝒰 ) → (𝒰 _0))) ((<function1>) (n : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )

scala> val pfDoubleEven =
     |   NatInd.induc(n :-> isEven(double(n))){
     |     zeroEven}{
     |       n :~> (
     |         hyp :-> (
     |           plusTwoEven(double(n))(hyp)
     |           )
     |           )
     |     } !: thmDoubleEven
pfDoubleEven: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = <function1>
```

## Indexed Inductive types

A generalization of inductive types are _inductive type families_, i.e., inductive types depending on an index.
Unlike parametrized inductive types (such as lists), the constructors of an inductive type family involve in general several different indices.
Further, the recursion and induction function only allow construction of (dependent) functions on the whole family.

A typical example is vectors, defined as a family indexed by their length.

```scala
scala> val IndN = new IndexedConstructorPatterns(Nat ->: Types)
IndN: provingground.IndexedConstructorPatterns[provingground.HoTT.Term,provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = provingground.IndexedConstructorPatterns@3e55cad6

scala> val Vec = "Vec" :: Nat ->: Type
Vec: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = Vec : ((Nat : 𝒰 ) → (𝒰 _0))

scala> val VecPtn = new IndexedConstructorPatterns(Nat ->: Types)
VecPtn: provingground.IndexedConstructorPatterns[provingground.HoTT.Term,provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = provingground.IndexedConstructorPatterns@52101fed

scala> val VecFmly = VecPtn.Family(Vec)
VecFmly: VecPtn.Family = Family(Vec : ((Nat : 𝒰 ) → (𝒰 _0)))

scala> val VecInd = {"nil" ::: VecFmly.head(Vec(zero))} |:  {"cons" ::: n ~>>: (A ->>: Vec(n) -->>: VecFmly.head(Vec(succ(n))))} =: VecFmly
VecInd: VecPtn.iConstructorSeq.Cons = Cons(iConstructorDefn(iW(((0 : (Nat : 𝒰 )) , (Star))),nil : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ),Vec : ((Nat : 𝒰 ) → (𝒰 _0))),Cons(iConstructorDefn(CnstDepFuncPtn(Nat : 𝒰 ,<function1>),cons : (∏(($p : (Nat : 𝒰 )) ↦ ((A : 𝒰 ) → (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ($p : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ($p : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))))),Vec : ((Nat : 𝒰 ) → (𝒰 _0))),Empty(Vec : ((Nat : 𝒰 ) → (𝒰 _0)))))

scala> val List(vnil, vcons) = VecInd.intros
vnil: provingground.HoTT.Term = nil : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 )
vcons: provingground.HoTT.Term = cons : (∏(($p : (Nat : 𝒰 )) ↦ ((A : 𝒰 ) → (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ($p : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ($p : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))))

scala> vcons.typ.fansi
res45: String = ∏(($p : Nat) ↦ A → Vec($p) → Vec(succ($p)))
```

We can define function recursively on vectors of all indices. For instance, we can define the size.

```scala
scala> val vn = "v_n" :: Vec(n)
vn: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = v_n : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (n : (Nat : 𝒰 )) : 𝒰 )

scala> val recVN = VecInd.rec(Nat)
recVN: VecInd.RecType = (RecSym(iConstructorDefn(iW(((0 : (Nat : 𝒰 )) , (Star))),nil : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ),Vec : ((Nat : 𝒰 ) → (𝒰 _0)))) : (Nat : 𝒰 )) ↦ ((RecSym(iConstructorDefn(CnstDepFuncPtn(Nat : 𝒰 ,<function1>),cons : (∏(($p : (Nat : 𝒰 )) ↦ ((A : 𝒰 ) → (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ($p : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ($p : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))))),Vec : ((Nat : 𝒰 ) → (𝒰 _0)))) : (∏(($t : (Nat : 𝒰 )) ↦ ((A : 𝒰 ) → (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ($t : (Nat : 𝒰 )) : 𝒰 ) → ((Nat : 𝒰 ) → (Nat : 𝒰 ))))))) ↦ (($s_1 : (Nat : 𝒰 )) ↦ (($s_2 : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ($s_1 : (Nat : 𝒰 )) : 𝒰 )) ↦ ((<function1>) ((($s_1 : (Nat : 𝒰 )) , ($s_2 : ((V...

scala> val size = recVN(zero)(n :~>(a :-> (vn :->(m :->(succ(m))))))
size: provingground.HoTT.Term = ($s_1 : (Nat : 𝒰 )) ↦ (($s_2 : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ($s_1 : (Nat : 𝒰 )) : 𝒰 )) ↦ ((<function1>) ((($s_1 : (Nat : 𝒰 )) , ($s_2 : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ($s_1 : (Nat : 𝒰 )) : 𝒰 )))) : (Nat : 𝒰 )))

scala> size(zero)(vnil)
res46: provingground.HoTT.Term = 0 : (Nat : 𝒰 )

scala> val v1 = vcons(zero)(a)(vnil)
v1: provingground.HoTT.Term = (((cons : (∏(($p : (Nat : 𝒰 )) ↦ ((A : 𝒰 ) → (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ($p : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ($p : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))))) (0 : (Nat : 𝒰 )) : ((A : 𝒰 ) → (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))) (a : (A : 𝒰 )) : (((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ) → ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))) (nil : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 )) : ((Vec : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0...

scala> size(one)(v1)
res47: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> assert(size(one)(v1) == one)
```

For a more interesting example, we consider vectors with entries natural numbers, and define the sum of entries.
```scala
scala> val VecN = "Vec(Nat)" ::: Nat ->: Types
VecN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] = Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))

scala> val VecNFmly = VecPtn.Family(VecN)
VecNFmly: VecPtn.Family = Family(Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0)))

scala> val vnn = "v_n" :: VecN(n)
vnn: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = v_n : ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) (n : (Nat : 𝒰 )) : 𝒰 )

scala> val VecNInd = {"nil" ::: VecNFmly.head(VecN(zero))} |:  {"cons" ::: n ~>>: (Nat ->>: VecN(n) -->>: VecNFmly.head(VecN(succ(n))))} =: VecNFmly
VecNInd: VecPtn.iConstructorSeq.Cons = Cons(iConstructorDefn(iW(((0 : (Nat : 𝒰 )) , (Star))),nil : ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ),Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))),Cons(iConstructorDefn(CnstDepFuncPtn(Nat : 𝒰 ,<function1>),cons : (∏(($l : (Nat : 𝒰 )) ↦ ((Nat : 𝒰 ) → (((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ($l : (Nat : 𝒰 )) : 𝒰 ) → ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ($l : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))))),Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))),Empty(Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0)))))

scala> val recVNN = VecNInd.rec(Nat)
recVNN: VecNInd.RecType = (RecSym(iConstructorDefn(iW(((0 : (Nat : 𝒰 )) , (Star))),nil : ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ),Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0)))) : (Nat : 𝒰 )) ↦ ((RecSym(iConstructorDefn(CnstDepFuncPtn(Nat : 𝒰 ,<function1>),cons : (∏(($l : (Nat : 𝒰 )) ↦ ((Nat : 𝒰 ) → (((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ($l : (Nat : 𝒰 )) : 𝒰 ) → ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ($l : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ))))),Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0)))) : (∏(($p : (Nat : 𝒰 )) ↦ ((Nat : 𝒰 ) → (((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ($p : (Nat : 𝒰 )) : 𝒰 ) → ((Nat : 𝒰 ) → (Nat : 𝒰 ))))))) ↦ (($o_1 : (Nat : 𝒰 )) ↦ (($o_2 : ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ($o_1 : (Nat : 𝒰 )) : 𝒰 )) ↦ ((<functio...

scala> val List(vnilN, vconsN) = VecNInd.intros
vnilN: provingground.HoTT.Term = nil : ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 )
vconsN: provingground.HoTT.Term = cons : (∏(($l : (Nat : 𝒰 )) ↦ ((Nat : 𝒰 ) → (((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ($l : (Nat : 𝒰 )) : 𝒰 ) → ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ($l : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))))

scala> val k ="k" :: Nat
k: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = k : (Nat : 𝒰 )

scala> val vsum = recVNN(zero)(n :~>(k :-> (vnn :->(m :-> (add(m)(k)) ))))
vsum: provingground.HoTT.Term = ($o_1 : (Nat : 𝒰 )) ↦ (($o_2 : ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ($o_1 : (Nat : 𝒰 )) : 𝒰 )) ↦ ((<function1>) ((($o_1 : (Nat : 𝒰 )) , ($o_2 : ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ($o_1 : (Nat : 𝒰 )) : 𝒰 )))) : (Nat : 𝒰 )))

scala> vsum(zero)(vnilN)
res49: provingground.HoTT.Term = 0 : (Nat : 𝒰 )

scala> val v2 = vconsN(zero)(two)(vnilN)
v2: provingground.HoTT.Term = (((cons : (∏(($l : (Nat : 𝒰 )) ↦ ((Nat : 𝒰 ) → (((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ($l : (Nat : 𝒰 )) : 𝒰 ) → ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ($l : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))))) (0 : (Nat : 𝒰 )) : ((Nat : 𝒰 ) → (((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ) → ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) (0 : (Nat : 𝒰 )) : 𝒰 ) → ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))...

scala> vsum(one)(v2)
res50: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> assert(vsum(one)(v2) == two)

scala> val v3 = vconsN(one)(one)(v2)
v3: provingground.HoTT.Term = (((cons : (∏(($l : (Nat : 𝒰 )) ↦ ((Nat : 𝒰 ) → (((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ($l : (Nat : 𝒰 )) : 𝒰 ) → ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ($l : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : ((Nat : 𝒰 ) → (((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 ) → ((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : 𝒰 )))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (((Vec(Nat) : ((Nat : 𝒰 ) → (𝒰 _0))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ...

scala> v3.fansi
res52: String = cons(succ(0))(succ(0))(cons(0)(succ(succ(0)))(nil))

scala> vsum(two)(v3)
res53: provingground.HoTT.Term = (succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) ((succ : ((Nat : 𝒰 ) → (Nat : 𝒰 ))) (0 : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )) : (Nat : 𝒰 )

scala> assert(vsum(two)(v3) == three)
```
