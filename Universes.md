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

### To avoid, and how:

* Using "logical universe" etc.
* __Especially__, the identity type cannot be having all function and induction types in the first universe.
* Avoid covariance issues in --> etc. by having parametrized methods and functions.
* As a term determines its type, avoid specifying both types and terms as parameters. If we need to say terms have a specific type, this can be a trait to be mixed in.

### Issues:

* Symbolic objects in various universes. __Solved:__ as all universes contain the first, the best we can do is have SymbTyp as the type in all cases. 