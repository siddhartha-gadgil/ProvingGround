---

title: Recursion and Contexts

---

## Recursion, Lambdas, Contexts.

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

### Contexts
* These are used to both interpret and construct functions easily.
* The basic ones are lambdas.
* In addition, such as in recursion, we can introduce objects that can be used but are not variables.
* We also introduce definitions, so objects are introduced that are given in terms of other objects,, but can be used directly.
* We also have anonymous objects, generally created by evolution, that can be added to the context. These can have weights on them.
* **Context events:** These change the context, based on both interpretation and  evolution.
* We have to also introduce definitions, including those based on cases.