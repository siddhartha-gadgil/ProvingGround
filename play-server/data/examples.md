# Examples for ProvingGround:
### To be eventually parsed and proved. Markdown headings are ignored.

## Modus Poens: A theorem, i.e.,
## we define a type and this has a representative
A : Type -> B : Type ->
A -> (A -> B) -> B

## Variant of Modus Poens:
(A: Type , B: Type) ->
A -> (A -> B) -> B

## Semigroup
G : SemiGroup := 
G : Set,
_ * _ : (G , G) -> G,
(require) (ForAll) a : G, b: G, c : G, (a * b) * c = a * (b * c)

## Monoid
G : Monoid := G : Semigroup,
(require) Exists 1 : G, 
(require) (ForAll) a: G, 1 * a = a * 1 = a

## Monoid
G: Monoid := 
G : Semigroup, 
1 : G,
(ForAll) a: G,  1 * a = a * 1 = a


## A theorem
G : Monoid ->
ForAll a: G ,
(ForAll b: G, a * b = b) -> a = 1

## Abelian Groups

Abelian : Group -> _
Abelian :=
  G: Group -> ForAll a, b: G, a * b = b * a 

## Order on Sets 
IsOrder: S: Set -> ((S, S) -> Type) -> Type
IsOrder = 
S: Set ->
_<=_ : (S, S) -> Type ->
Type : S
(require) a <= a,
a <= b, b <= a -> a = b
a <= b, b <= c -> a <= c


## Total order on a set
IsTotalOrder : S: Set -> _<=_ : Order(S) -> Type
IsTotalOrder = a : S, b: S -> (a <= b) + (b <= a)

## Order: Better version
Order :=
(S: Set) ->
_<=_ : (S, S) -> _,
a: S -> a <= a,
a, b: S -> (a <= b), (b <= a) -> (a = b)
a, b, c: S -> (a<=b) , (b <=c) -> (a <= c)

## Total order: Better version
TotalOrder := 
(S: Set)-> 
Order(S),
a, b: S -> a <= b | b <= a
 

 

