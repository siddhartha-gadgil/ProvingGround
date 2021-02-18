# Chomp session selecta

The initial goals, the one remaining from the chomp and its negation, were:

```scala
SeekGoal((`@a : 𝒰 _0 ) ~> (((𝒰 _0) → (((`@a) , (𝒰 _0)))) → (`@a)),{},Vector())
SeekGoal(∑((``@a :  𝒰 _0) ↦ ((((𝒰 _0) → (((``@a) , (𝒰 _0)))) , ((``@a) → (Zero))))),{},Vector())
```

The first of these is true, but complicated. With two lamdbas, we get a variable of type `((𝒰 _0) → (((@a) , (𝒰 _0)))`. We apply this to any type, say `One`, and project. 
In reality, all the goals we seek in this branch are the following:

```scala

```