---
layout: post
title: On import from Lean Export format
date: 2017-10-17
---

For the last few weeks/months, a focus of my work was to import from _lean theorem prover_ using its export format. As I move away from this, its time to record some lessons.

### Universes

* Lean uses universe parametrizations.
* For the first time in my code, which universe mattered, with function application failing because the domain and the type of the argument differed in Universes.
* An unsafe hack of making universes equal was implemented.
* Eventually, this should be replaced by an appropriate unified _apply_ function.

### Propositions

* Lean makes use of Propositions, which are at level _-1_.
* Some of the difficulty was in the way propositional witnesses are suppressed - just a pain with no lessons.
* But the main step was implementing propositional types: a universe _Prop_, symbolic types in this universe, and the definition that symbolic objects with type a proposition are all just named _witness_, and hence equal.
* This is in general a worthwhile optimization for equality and even function application.

### Recursion paralysis

* Lean uses a lot of recursion, going very deep.
* This uncovered some bugs in replacement (where it was trivial) which have been fixed.
* Nevertheless, recursion remains very slow. One can partly blame _lean export_, but it is worth trying to fix this.
* The key steps in recursion, including _replacement_ and _equality_, are done without any concurrency or stack safety.
* One should presumably wrap and override these with, for example, _Task_. 
