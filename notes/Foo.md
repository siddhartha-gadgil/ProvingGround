Here is how you add numbers:
```scala
scala> 1 + 1
res0: Int = 2
```

Let's do it with Jupyter
```scala
import $ivy.`in.ac.iisc::provingground-core:0.1-SNAPSHOT`
import provingground.HoTT._
val A = "A" :: Type
```
We get the results

```scala
import $ivy.$                                            

import provingground.HoTT._

A: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A : 𝒰 _0
```

Just to check the kernel stays in scope.

```scala
val B = "B" :: Type
```

with results
```
B: Typ[Term] with Subs[Typ[Term]] = B : 𝒰 _0
```

```scala
// %%
println("this is a cell")
val z = 2
println(z + z)
```

with results

```
this is a cell
4
z: Int = 2
```
