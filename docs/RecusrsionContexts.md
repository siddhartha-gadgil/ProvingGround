---

title: Recursion and Contexts

---

## Recursion, Lambdas, Contexts.

### Details of Recursion contexts
* For each constructor and a list of variables, we get a context.
* The RHS of the corresponding definition, on applying elim, gives an element of the type of this context. We can store this as a map.
* We associate a variable to each of the constructor context - taking a lambda with each of these gives a context with type the _recursion function_.
* We can fold in the terms given by applying elim to each RHS to get the defintion of _f_.  
* __Question:__ How do we incorporate the identities.
* For generality, constructors should not be referred to by name, but as actual constructors (that are generally functions).
* These are associated to PolyPatterns, and have their type, but may not be symbolic objects - or we use a class for constructors.
* __Conceptual issue:__ The context constructed depends on _f_, but not the recursion function itself. The HoTT type of the context, however, is independent of _f_, so a symbolic object of this type is universal.



## Older observations

### Patterns:
* The simplest pattern: define _f_ by
    - f(x) = g(x), i.e.
    - f : x :-> g(x)
    - This is an ordinary lamdbda, which we make a lambda context.
* Curried function _f_:
    - f x y = g(x, y), i.e.
    - f x : y :-> g(x, y), i.e.
    - f : x :-> y :-> g(x, y)
* A pattern for recursion:
    - f (succ x) = g(x) becomes
    - f : (succ x) :-> g(x).
    - This is like a lambda, except the left hand side is not a variable.
    - We rewrite such patterns too:
    - f : succ :-> x :-> g(x)
    - This gives  a definition of an induced function by f.

### Recursion and Induction concepts:
* Conceptually (in HoTT), recursion and induction are applications of functions, that are part of inductive type defintions.
* These functions are those induced by a function _f_ being defined.
* Thus, we can give the function _f_ in terms of these induced functions, by applying the appropriate _rec_ or _ind_ function.
* Each induced function is given by pattern matching of the above form.
* The equality relations for functions just constructed are added to the context.
* For each pattern, we associate a context so that the _elim_ value is the function. This must have the function being defined passed by name to the context.

### Contexts
* These are used to both interpret and construct functions easily.
* The basic ones are lambdas.
* In addition, such as in recursion, we can introduce objects that can be used but are not variables.
* We also introduce definitions, so objects are introduced that are given in terms of other objects,, but can be used directly.
* We also have anonymous objects, generally created by evolution, that can be added to the context. These can have weights on them.

### Contexts: get methods
* To define functions using cases etc. we not only have methods to change contexts and export from them, but also to get a specific object (given by a definition).
* This is an option type, defined recursively.
* We should also be able to get a list of all defined objects.



