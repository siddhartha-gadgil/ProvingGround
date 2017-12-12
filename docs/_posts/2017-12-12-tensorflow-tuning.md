---
title: Tuning with Tensorflow
date: 2017-12-12
layout: post
---

While the _discovery_ process involves actual manipulation of terms, we refine using a _reflection_ process, involving working with a large list of already discovered terms and their relations. The goal of this process is to efficiently encode mathematical knowledge, and especially to identify important theorems and concepts as well their relations.

# The basic case: generating proofs.

Assume that we are given a distribution on _types_ and we wish to find the _best_ generating model.

## Best model
* Best is based on:
  * _cross-entropy_ between the mapped distribution from terms and the given distribution on types.
  * _cost_ of generation, measured in terms of both the entropy of the initial distribution and the parameters involved in generating this.
  * the generated distribution depends on the _cutoff_, which may be taken as part of the cost (though this may not work well with tuning).

* The model itself is:
  - an initial distribution
  - a recursive formula giving an evolved distribution,
  - parameters for the recursive definition.
  - a more sophisticated model may have vectors for terms and choosing etc based on linear transformations.


## The Tensorflow graph

We build this graph automatically, with a recursive definition. The entities are:

* Variables
  - for terms in the initial distribution,
  - for parameters of the evolution,
  - for various total probabilities, to be used in conditioning.

* Placeholders/constants: for the generated probabilities of types.

* Calculated expressions:
  - the _final_ (given cutoffs) probabilities of terms.
  - for the _final_ probabilities of types.
  - for the actual total probabilities corresponding to the conditions.
  - entropies and other costs for generation.
  - cross-entropies or other matching measures.
  - cost of mismatch of total probability for conditions.

* Optimization cost: combination of
  - cost of generation
  - matching measure,
  - penalty for identities for total probabilities.

* Recursive definitions:
  - expressions for terms,
  - variables: should have new ones for _conditioning_, and perhaps also for discovered terms.
  - weight of a term if it were to be introduced in the initial distribution, as a function of the term.

* Multi-stage discovery:
  - get an expression for the weight of a new term in terms of earlier terms.
  - by substitution, we get expressions in terms of the initial terms (as also the variables for the new terms).
  - should also introduce _witnesses_.
