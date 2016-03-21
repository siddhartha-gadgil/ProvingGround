# Natural Language to HoTT expressions

## Overview

### Target Language

The target language will be a structure (collection) built from elements inspired by _Lean Export Language Expressions_. The document structure - sentences, paragraphs and explicit structure such as sections will be preserved.

The lean expression language itself is very small, with expressions built from:

* lambda's
* Pi's
* Function applications.
* Constants and Variables (variables from lambdas).

As this is small enough, it is more convenient to build a custom language. This should allow

* functions applied to quantified expressions such as _some prime_, i.e., inner quantification.
  * A more complex such expression is 2 primes.
* Heads of lambda/pi expressions.
* the inner quantification can be viewed as the head of pi or sigma - for this sigma has to be part of the language.
* Hence, perhaps, all we have to add are:
  * lambda-head - which gives pi-types as well.
  * pair(x) - which gives sigma-types.
  * with these commuting in unusual ways.
  * allowing interpretation as types by picking witnesses to statements.  

**Use Type-classes:**

* Whether a built expression is valid, especially for function applications, depends on details of interpretation, especially types.
* Hence it is not good to just have formal expressions.
* Instead we define a type-class for being able to build expressions.
* We may even factor into different type-classes, for instance with _some_ and _all_ separated from the core operations.
* Further, we can have more evolved type-classes with Pi's and lambda's (not just fragments) built implicitly from the basic ones.
* As the source language is likely to be fixed, we do not abstract for now.

### Source

* The raw source will be wikipedia and latex files. This is processed by the Stanford NLP tools to give _constituent parse_ trees, as well as _ner_, _coref_ etc.
* In addition, the source contains latex. This has to be parsed separately, with formulas being replaced by dummy proper nouns (_Capitalized_ names) before parsing.

### Recursive parsing

For the construction of Lean expressions, we have a few builders. The source languages (nlp data and latex) are defined by abstract patterns, i.e., _unapply_ methods. However, it is worth doing everything cleanly with higher-kinded types, rather than the ad hoc recursive translation.

Specifically, we target  and build from types X[A] where

* X[A] is a functor.
* We can flatten X[Option[A]] to Option[X[A]] - this is needed as the lift of a parser A => B gives X[A] => X[Option[B]].

We also need to flatten Option[Option[A]] to Option[A]. We may have flatMaps that combine functoriality with flattening.

The lifts we actually use are:

* From A => Option[B] we get X[A] => Option[X[B]].
* From C => Option[D] we get Option[C] = > Option[D] (ordinary flatMap).

### Stochasticity: correcting for incorrect parse trees.

* We can make only the builders stochastic.
* __Question:__ Can this be achieved by just taking the output language to be probability distributions?

## Rules for parsing

* A noun phrase, verb phrase can represent function application, with the verb being interpreted as a function.

## Subtleties:

* Quantifiers can be part of expressions, such as in _a prime_, _some number_, _every manifold_.

## Notes from Examples

* Parsing of _if_ is fragile, depending on the presence of then. We can get one of two results:
  * In the absence of then, the sentence has two equal parts (plus the parenthesis), with an if clause. The first clause should be interpreted as an incomplete *P -> _*.
  * If then is present, we get _if_ dominating with obvious clauses to combine.

## Examples:

* ``if a prime number P divides the product of M and N, then P divides one of M and N.``
```
(ROOT
  (SBAR (IN if)
    (S
      (S
        (NP (DT a) (JJ prime) (NN number) (NN P))
        (VP (VBZ divides)
          (NP
            (NP (DT the) (NN product))
            (PP (IN of)
              (NP (NNP M)
                (CC and)
                (NNP N))))))
      (, ,) (RB then)
      (S
        (NP (NNP P))
        (VP (VBZ divides)
          (NP
            (NP (CD one))
            (PP (IN of)
              (NP (NNP M)
                (CC and)
                (NNP N)))))))))
```

* ``Every natural number greater than 1 is divisible by a prime number``

```
(ROOT
  (FRAG
    (NP
      (NP (DT Every) (JJ natural) (NN number))
      (SBAR
        (S
          (NP
            (QP (JJR greater) (IN than) (CD 1)))
          (VP (VBZ is)
            (ADJP (JJ divisible)
              (PP (IN by)
                (NP (DT a) (JJ prime) (NN number))))))))))

```

* ``Every natural number N greater than 1 is divisible by a prime number``

```
(ROOT
  (S
    (NP
      (NP (DT Every) (JJ natural) (NN number))
      (VP (VBG N)
        (NP
          (QP (JJR greater) (IN than) (CD 1)))))
    (VP (VBZ is)
      (ADJP (JJ divisible)
        (PP (IN by)
          (NP (DT a) (JJ prime) (NN number)))))))

```


* ``Every natural number which is greater than 1 is divisible by a prime number``

```
(ROOT
  (S
    (NP
      (NP (DT Every) (JJ natural) (NN number))
      (SBAR
        (WHNP (WDT which))
        (S
          (VP (VBZ is)
            (NP
              (QP (JJR greater) (IN than) (CD 1)))))))
    (VP (VBZ is)
      (ADJP (JJ divisible)
        (PP (IN by)
          (NP (DT a) (JJ prime) (NN number)))))))

```

* ``Every natural number N which is greater than 1 is divisible by a prime number``

```
(ROOT
  (S
    (NP
      (NP (DT Every) (JJ natural) (NN number) (NNP N))
      (SBAR
        (WHNP (WDT which))
        (S
          (VP (VBZ is)
            (NP
              (QP (JJR greater) (IN than) (CD 1)))))))
    (VP (VBZ is)
      (ADJP (JJ divisible)
        (PP (IN by)
          (NP (DT a) (JJ prime) (NN number)))))))

```

* ``if a prime number P divides MN, P divides one of M and N``

```
(ROOT
  (S
    (SBAR (IN if)
      (S
        (NP (DT a) (JJ prime) (NN number) (NN P))
        (VP (VBZ divides)
          (NP (NNP MN)))))
    (, ,)
    (NP (NNP P))
    (VP (VBZ divides)
      (NP
        (NP (CD one))
        (PP (IN of)
          (NP (NNP M)
            (CC and)
            (NNP N)))))))

```

* ``if a prime number P divides MN then P divides one of M and N``

```
(ROOT
  (SBAR (IN if)
    (S
      (NP (DT a) (JJ prime) (NN number) (NNP P) (NNP divides) (NNP MN))
      (ADVP (RB then))
      (VP
        (ADVP (RB P))
        (VBZ divides)
        (NP
          (NP (CD one))
          (PP (IN of)
            (NP (NNP M)
              (CC and)
              (NNP N))))))))

```

* ``Every even number which is greater than two is the sum of two primes``

```
(ROOT
  (S
    (NP
      (NP (DT Every) (JJ even) (NN number))
      (SBAR
        (WHNP (WDT which))
        (S
          (VP (VBZ is)
            (NP
              (QP (JJR greater) (IN than) (CD two)))))))
    (VP (VBZ is)
      (NP
        (NP (DT the) (NN sum))
        (PP (IN of)
          (NP (CD two) (NNS primes)))))))

```

* ``Six is not the sum of two distinct primes``

```
(ROOT
  (S
    (NP (CD Six))
    (VP (VBZ is) (RB not)
      (NP
        (NP (DT the) (NN sum))
        (PP (IN of)
          (NP (CD two) (JJ distinct) (NNS primes)))))))

```

* ``zero is less than or equal to every natural number``

```
(ROOT
  (S
    (NP (CD zero))
    (VP (VBZ is)
      (ADJP
        (ADJP (JJR less)
          (PP (IN than)))
        (CC or)
        (ADJP (JJ equal)
          (PP (TO to)
            (NP (DT every) (JJ natural) (NN number))))))))

```

* ``The image of Z in K is an integral domain, hence isomorphic to Z or Z over p, where p is a prime.``

```
(ROOT
  (S
    (NP
      (NP (DT The) (NN image))
      (PP (IN of)
        (NP
          (NP (NNP Z))
          (PP (IN in)
            (NP (NNP K))))))
    (VP (VBZ is)
      (NP
        (NP (DT an) (JJ integral) (NN domain))
        (, ,)
        (ADVP (RB hence))
        (ADJP (JJ isomorphic)
          (PP (TO to)
            (NP
              (NP (NNP Z)
                (CC or)
                (NNP Z))
              (PP (IN over)
                (NP
                  (NP (NN p))
                  (, ,)
                  (SBAR
                    (WHADVP (WRB where))
                    (S
                      (NP (NN p))
                      (VP (VBZ is)
                        (NP (DT a) (JJ prime))))))))))))
    (. .)))

```

* ``if a prime number P divides MN, P divides one of M and N``

```
(ROOT
  (S
    (SBAR (IN if)
      (S
        (NP (DT a) (JJ prime) (NN number) (NN P))
        (VP (VBZ divides)
          (NP (NNP MN)))))
    (, ,)
    (NP (NNP P))
    (VP (VBZ divides)
      (NP
        (NP (CD one))
        (PP (IN of)
          (NP (NNP M)
            (CC and)
            (NNP N)))))))

```
