---
layout: post
title: Computer assistance in Homogenous length functions
date: 2018-01-14
---

The role of the computer (used by me) was to find a proof of an (at that time best) concrete bound on the length of the commutator of generators, and output this in a (barely) human readable form as [posted](https://github.com/siddhartha-gadgil/Superficial/wiki/A-commutator-bound). Pace Nielsen read through the proof and saw a pattern, with the "same" conjugacy and the "same" pair of triangle inequalities being repeatedly applied in this proof. This was used by him to get strong bounds, and then he and others refined and abstracted this to get the _splitting lemma_ in the paper.

There were two limitations of the way the computer proof was done:

* While the use of conjugacy invariance and triangle inequality was optimal and algorithmic, of which elements to take powers was manually specified by me. This should have been made smart, and would have soon enough except the extreme smartness of the people in this polymath group made this redundant (problem was solved within 24 hours of the first posted computer proof).
* More importantly, I used [domain specific foundations](https://github.com/siddhartha-gadgil/Superficial/blob/master/src/main/scala/freegroups/LinNormBound.scala), which could encode only one kind of proof, that a specific word has length bounded by a specific number. This rules out in particular both firmulas for bounds that are quatified (and so must invlove variables) and recursion/induction. To show that such results can be at least _encoded_ I formalized the [internal repetition trick](http://siddhartha-gadgil.github.io/ProvingGround/tuts/LengthFunctions.html).

More generally, where a computer helped was in following instructions of the form "try these method in lots of cases in lots of ways and give me the best proof for thess cases (or where we got a strong result)". It is obvious that the "lots of cases" and "lots of ways" are much bigger numbers for computers than by hand. The question is how general one can be with "these methods". I do think even in practice a lot of methods can be encoded, and  this is underutilized as people underestimate this. In principle, in the era of Homotopy type theory and Deep learning, presumably every method can be encoded.
