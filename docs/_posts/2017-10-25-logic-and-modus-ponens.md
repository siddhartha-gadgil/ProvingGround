---
title: Logic and Modus Ponens
date: 2017-10-25
layout: post
---

* _Logic_ in HoTT is essentially generation from just types.
* For instance, we can take a starting point `A, B : Type`.
* We have essentially only three results (in the absence of negation):
    - Identity: $A \\to A$
    - Constant function: $A \\to B \\to A$
    - Modus ponens: $A \\to (A \\to B) \\ to B$.
* Our task is to discover these results and identify them as the only significant ones.
* More accurately, the only significant ones should be the abstractions of these, with $A$ and $B$ variables.

### Discovery: Works

* All these are not consequences of other results, so must be discovered at the first level.
* Indeed, they all are if the decay is high enough, $cutoff = 10^{-6}$ and $decay = 10$ works fine, as does a range.
* We should experiment starting with just `Type`.

### Refinement: Work to be done

* The results of exploration also give dependence between theorems.
* This is true even if a theorem is discovered directly, dependence is seen by having a higher weight in a later stage.
* This can even be _joint dependence_, i.e., a boost for two perturbations together.
* For this to work, we need to
    - consider as tangents (partial and total) $\\lambda$-closures of terms.
    - ensure all distributions are normalized so numbers are comparable.
* We thus get some backward propagation, but limited to new _propositions_, not just, say, a useful function.
