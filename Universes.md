---

title: Universes in ProvingGround and HoTT

---

## Universes in Proving Ground and HoTT

### Specifying universes:

* We can index universes by natural numbers (integers requiring positivity).
* Each universe is contained in the next, so we need not be too specific.
* We have to specify the universe obtained by constructions: functions types, Pi-types, inductive types and type families.
* Important example: iterated equality types are in higher universes, so we cannot just assume everything is in the first universe.
    - This  is an inductive type, so we should specify rules for inductive type families. 
    - If a and b are objects of a type A, then a=b is a type of the same level as A.
    - So (a=b)=(c=d) is now at the same level as the _type_ of A. So its type is a higher universe.
    - In terms of inductive types, this is consistent with taking the maximum over all constructors and parameters. It cannot be better than this, as we can otherwise just warp a type and put it in a lower universe.
    - The distinction between parameters and indexes may not be important, so inductive types and type families may be treated the same way.
* In code, specify that the type of a type is a universe.

### To avoid, and how:

* Using "logical universe" etc.
* __Especially__, the identity type cannot be  having all function and induction types in the first universe.
* Similarly use maxima to describe universes for other inductive types: products, sums etc. and general inductive ones.
* Avoid covariance issues  by having parametrized methods and functions.
* As a term determines its type, avoid specifying both types and terms as parameters.

### Hierarchy and scala code:

* As the universes are cumulative, we can only ensure that the  objects of a universe (even a higher one) are types.
* This is true at the  level of HoTT as well, so there is no loss in expressiveness in not making more refined  statements.
* Hence it suffices to make Symbolic Types with parameter _Term_ only, and have these as symbolic objects in all universes.