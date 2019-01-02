## When types are not substituted

Sigma-type
- should be defined property *** FAILED ***
```scala
  ((a : A ) ~> ((ba : (B(_ : A)) (a) ) ~> (((D(_ : Σ(a : A, B(a)))) (a)) (ba)))) → ((((aa) , (bbaa)) : ∑((a :  A) ↦ ((B(_ : A)) (a))) ) ~> (((D(_ : Σ(a : A, B(a)))) (aa)) (bbaa))) 
  did not equal 
  ((a : A ) ~> ((ba : (B(_ : A)) (a) ) ~> (((D(_ : Σ(a : A, B(a)))) (a)) (ba)))) → (((((a, ba)_1) , ((a, ba)_2)) : ∑((a :  A) ↦ ((B(_ : A)) (a))) ) ~> (((D(_ : Σ(a : A, B(a)))) ((a, ba)_1)) ((a, ba)_2))) (IntegrationSpec.scala:667)
```