---
title: Proving Ground
layout: default
---

This is a system under development for automated theorem proving. More concretely, we take as the goal of automated theorem proving _to equip computers with all the major capabilities used in discovering and proving mathematical results_.  Our goal is to complement the many software systems that are useful in doing mathematics, with a focus on missing capabilities.

The one success of this project so far has been its role in the [Polymath 14 project](http://math.iisc.ac.in/~gadgil/presentations/HomogeneousLengths.html).

## Foundations

The foundations we use are **Homotopy Type Theory** combined with symbolic algebra. This is crucial to our approach as the result is proofs that are not much more complex than real world mathematics - as illustrated, for example, in the [PolyMath lemma](tuts/internal-repetition-for-length-functions.html). Note that much of that note is the fully expanded proofs as reported by the system - the code describing the proofs is not long.

The conciseness of HoTT proofs is not only of practical value, but is conceptually important as we base learning on the complexity of mathematical proofs.

Much of the work done so far has been on the implementation of homotopy type theory in scala, with an attempt to capture in scala types significant amount of information about the HoTT types.

## Learning and reflection

The system learns based on a generative model that not only matches the generated distribution with a desired one, but takes into account the complexity of the generative model. This allows one to judge whether newly discovered results are worthwhile.

## Lean theorem prover interaction

Our goal is to interact with external systems. In particular, we have implemented interaction with the _lean theorem prover_, whereby the export format of _lean_ is used (via trepplein) to generate code in our implementation of HoTT. This allows us to start with a library for learning as well as a target for natural lannguage processing.


## Running

At present the best way to interact with the code is to use a console in either [mill](https://www.lihaoyi.com/mill/) or `sbt` (the primary build tool is now [mill](https://www.lihaoyi.com/mill/)). To pop up a console with most of the code in scope, install [mill](https://www.lihaoyi.com/mill/) and run:
```
mill mantle.repl
```

A server to experiment with (or even use) various parts of the code will be available soon.
