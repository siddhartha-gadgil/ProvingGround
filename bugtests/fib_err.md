## REPL session with tracking:

```scala
@ val ferrfn = ffib_aux(succ(succ(n)))
Matched Func case (printing from HeadData): key : 5450
Recursion data (n : (Nat : 𝒰 _0)) ↦ ((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) ↦ ((m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ (((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))
 of type: (Nat : 𝒰 _0) → (((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) → ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))
Argument: (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)
Induced function co-incides: true

Matched Func case (printing from HeadData): key : 9999
Recursion data (n : (Nat : 𝒰 _0)) ↦ ((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) ↦ ((m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ (((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))
 of type: (Nat : 𝒰 _0) → (((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) → ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))
Argument: n : (Nat : 𝒰 _0)
Induced function co-incides: true

 Recursive result for key 9999: (<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))

 Result for key 9999 : (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))


Recursive Def case (println from RecDefCase)
Argument is n : (Nat : 𝒰 _0) for constructor succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))
Result is the term (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))


Matched Func case (printing from HeadData): key : 9292
Recursion data (n : (Nat : 𝒰 _0)) ↦ ((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) ↦ ((m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ (((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))
 of type: (Nat : 𝒰 _0) → (((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) → ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))
Argument: n : (Nat : 𝒰 _0)
Induced function co-incides: true

 Recursive result for key 9292: (<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))

 Result for key 9292 : (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))


Recursive Def case (println from RecDefCase)
Argument is n : (Nat : 𝒰 _0) for constructor succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))
Result is the term (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))



 Recursive result for key 5450: (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))

 Result for key 5450 : (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))


Recursive Def case (println from RecDefCase)
Argument is (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0) for constructor succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))
Result is the term (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))


Matched Func case (printing from HeadData): key : 8080
Recursion data (n : (Nat : 𝒰 _0)) ↦ ((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) ↦ ((m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ (((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))
 of type: (Nat : 𝒰 _0) → (((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) → ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))
Argument: (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)
Induced function co-incides: true
Matched Func case (printing from HeadData): key : 4152
Recursion data (n : (Nat : 𝒰 _0)) ↦ ((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) ↦ ((m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ (((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))
 of type: (Nat : 𝒰 _0) → (((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) → ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))
Argument: n : (Nat : 𝒰 _0)
Induced function co-incides: true

 Recursive result for key 4152: (<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))

 Result for key 4152 : (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))


Recursive Def case (println from RecDefCase)
Argument is n : (Nat : 𝒰 _0) for constructor succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))
Result is the term (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))


Matched Func case (printing from HeadData): key : 7781
Recursion data (n : (Nat : 𝒰 _0)) ↦ ((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) ↦ ((m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ (((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))
 of type: (Nat : 𝒰 _0) → (((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) → ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))
Argument: n : (Nat : 𝒰 _0)
Induced function co-incides: true

 Recursive result for key 7781: (<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))

 Result for key 7781 : (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))


Recursive Def case (println from RecDefCase)
Argument is n : (Nat : 𝒰 _0) for constructor succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))
Result is the term (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))



 Recursive result for key 8080: (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))

 Result for key 8080 : (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))


Recursive Def case (println from RecDefCase)
Argument is (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0) for constructor succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))
Result is the term (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))


ferrfn: Func[Term, Func[Term, Term]] = (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))
```


## Manual stepwise recursion

This does not seem to give errors at the same stage, but `data` gets perturbed at a later stage: (omitting debug output). We can check that the first function is fine (at least working in this order), and the result looks fine, but has the wrong formal function.

```scala
@ val recres = ffib_aux(succ(n))
recres: Func[Term, Func[Term, Term]] = (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))
@ val psi = fn(fn(fn(recres(m1)(m2))))
psi: Term = <function1>
@ psi == ffib_aux
res33: Boolean = true

@ val result = fstepData(succ(n))(recres)  
result: Func[Term with Subs[Term], Func[Term with Subs[Term], Term]] = (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((<function1>) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))

@ result(m1)(m2) == ffib_aux(n)(add(m1)(m2)){add(m2)(add(m1)(m2))}
res37: Boolean = false
@ fn(fn(fn(result(m1)(m2)))) == psi
res41: Boolean = false
@ val eta = fn(fn(fn(result(m1)(m2))))
eta: Term = <function1>
@ dc(eta)
res43: RecursiveDefinition.DataCons[u, v, w] = <function1>
@ dc(eta).data
res44: w = (m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ (m1 : (Nat : 𝒰 _0)))
@ dc(dc(eta).tail).data
res45: w = (n : (Nat : 𝒰 _0)) ↦ ((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ ((((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) ↦ (((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))
@ dc(dc(eta).tail).data == dc(dc(psi).tail).data
res46: Boolean = false
@ dc(dc(psi).tail).data
res47: w = (n : (Nat : 𝒰 _0)) ↦ ((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) ↦ ((m1 : (Nat : 𝒰 _0)) ↦ ((m2 : (Nat : 𝒰 _0)) ↦ (((fib_aux(n,_,_) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m2 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((add : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) (m1 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m2 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))
```
