---
title: Prover Components and Identities in a Monoid
date: 2017-10-20
layout: post
---

### Mathematical goal

Given a multiplication operation with left and right identities $e_l$ and $e_r$, it is a basic but non-trivial result that $e_l = e_r$. The proof is:

1. from $e_l * x = x$ deduce $e_l * e_r = e_r$.
2. from $x * e_r = x$ deduce $e_l * e_r = e_l$.
3. (Lemma) by symmetry of equality, from (2) deduce $e_l = e_l * e_r$.
4. (Theorem) by transitivity of equality, from (3) and (1) deduce $e_l = e_r$.

### Deduction principles

The full proof has very low weight to be discovered directly. Finding the proof illustrates, and tests, some key principles.

* We must judge the value of intermediate results.
* A good way of doing this is comparing weights of terms and types.
* Use unified application of functions.
* Use truncation of recursively defined probability distributions.

An earlier attempt, that sort of succeeded but took a couple of hours, was based on slowly tuning and using sampling. Subsequently, we used a hand-run version of the final method. Now all this has been encapsulated in code.

This is purely a forward search, though crucially with _lemma discovery_ along the way.

### Script and results

Here is the ammonite script that finds a proof:

```scala
val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)

def seek(n: Double = 6) = {
  val s = theoremSearchTask(dist1, tv, math.pow(10.0, -n), 3.minutes, eqM(l)(r), decay = 10)
  val f = s.runAsync
  f.foreach{
    case Some(t) => println(s"Found Proof ${t.fansi}\nfor Theorem ${t.typ.fansi}")
    case None => println("could not find theorem")
  }
  f
}
```

* Running with `seek` outputs the proof.
* We can change the parameter `n`: the result is found for `n = 5.2` and bigger. As `n` becomes larger running becomes slow, but the result is still found for `n = 7` on _snark_, and even `n = 10` on _sparrow_ (which has 20GB memory for the JVM).

### What is a lemma?

A proposition is a type with a witness. We can associate to it two generation probabilities, `p` of the statement and `q` of proofs - obtained using the _term to type_ map. We use a _parsimomy based entropy_,
`h(p, q) = -p/ (q * log(q))` and give weights to lemmas based on this.

We deduce consequences of lemmas, allocating resources depending on the weight.

### Breadth first search

We use an abstract breadth first search method for goals based on a type `X`, using Monix Tasks. This depends on:

* An initial vector of tasks (allow this for the sake of recursion) : `tv: Task[Vector[Task[X]]]`
* A goal function `X => Option[Y]`
* `spawn: Int => X => Task[Vector[Task[X]]]` for spawning new tasks
* a depth, used for recursion to prvent blowing up.

At each stage, first we look if the goal is attained across all the tasks. If not, we spawn tasks from each task. If the set of spawned tasks in empty, we terminate with `None`. Otherwise we recurse, but incrementing depth.

### Lemmas and theorems

Our first task is built using a `TermEvolver` to recursively define a probability distribution, and then evolving by `Truncate.task` with a cutoff. Lemmas are evaluated as above.

For subsequent steps, we generate using the _derivative_ component with a term evolver, with cutoff based on the weight of the lemma, and higher cutoffs at greater depth.

### Further variations

At an abstract level, instead of searching for goals, we should consider:

* Branches where we assess and accumulate progress, continuing while there is progress.
* Flowing to refine generating distributions, parameters etc, again terminating when there is not enough progress.
* Blended instead of branched flows, for instance a gradient flow.
* Repeatedly looping with increased sensitivity, ensuring there is enough time.
* Generating streams such as an `Iterant` along with tasks.
* Treating time remaining as a resource, and using this to decide parameters.

#### Remarks

* We can try to use the _result_ to include not just the proof, but lemmas along the way. This will fit in a breadth-first search, but one where the _goal_ function _p_ can be modified in spawned processes.
* Even for backward reasoning etc, having modified goals in sub-processes is natural.
* We can replace `Vector[X]` by any Monad `F[X]`, with simple flows corresponding to the `Id` Monad.
* The _goal_ function can really be a _not-enough-progress_ function which returns _only_ the accumuated successes (though this should not be used with branches).
* Simpler than modifying goals, we can modify the _state_ - successfully used to trace the above proof.


### Updates

* There was a deficiency in the above version: `decay` was applied with power `depth`, which meant no decay at the first step.
* This has been corrected, so `decay` has power `depth + 1`; the default decay in the Monoid task was changed to `3`
* Results still hold as above; but the correction allows modus ponens to be found.
