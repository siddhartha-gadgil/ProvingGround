---
title: Improvements to searching and  exploration
date: 2017-10-31
layout: post
---

A few improvements have been made over the versions that discovered _modus ponens_ and $e\_l = e\_r$

### Lambda-closures

If variables are specified, when an interesting lemma is discovered that depends on these, in addition to taking the tangent with respect to these, all partial _lambda-closures_ are considered.

### Scaling down cut-offs, relative scaling

* Previously, cutoffs were decided independently, which increased the chance of blowing up as a large number of processes with low cutoffs could be spawned.
* Now it is ensured that the total of the reciprocal of the cutoffs is at most the reciprocal of the original cutoff.
* Further, a scale factor decides the relative weights (hence cutoffs) - we make this high to increase sensitivity to the importance of lemmas.

### Tracing, avoiding duplication of tangents

* A method was added to explore with traces.
* While seeking and exploring with traces, using the same term for a tangent twice is avoided.


## Results

* Both $e_l = e_r$ and _modus ponens_ are still discovered, and in both seek and explore.
* However, $e_l = e_r$ is discovered using _two_ lemmas.
