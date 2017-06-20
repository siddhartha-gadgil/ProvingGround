# Natural language mathematics based on Naproche

Our goal is to translate human mathematical literature into semanticaly represented mathematics, eventually in HoTT, for the sake of _learning_, hence allowing some error and multiple possible translations. Here we summarize many components of the language of mathematics as found in the wild, based on both the Naproche CNL and the concluding section of Cramer's thesis about missing idioms.

In addition to this, real life mathematics concerns _attention_ and _relations_.

### Quantterms and pronouns

Quantterms are $n$, $n_k$ etc, and function like pronouns. In addion, singular  and plural 'it' are used in mathematics.

### Special kinds of sentences

Some of these are:

* Simple declarative sentence
* Assertion
* Assumption
* Definition
* Variable type specification
* Alternative notation specification

Many of these are more general in real life than in Naproche.

### Structured blocks

* Axiom blocks
* Assumption-consequences blocks
* Theorem-proof blocks
* Definition blocks
* Case distinction blocks
* Statement list blocks
* Note blocks
* Labelled text blocks

### Fuzzy structure

Many phrases and sentences (which are not in Naproche CNL) are to mark structure, for instance that to start, conclude or resume a proof.

Many kinds of blocks are fuzzy in real life. For example,

* A proof may be _inline_, so ends by conclusion.
* rather than having an explicit _case distinction_ block, one may have just case triggers. The end of the case can be marked by a conclusion in that case, or the start of a new case.
* An assumption-consequences block is where an assumption holds. This can be ended by ending the paragraph or section, by making a confliciting assumption, by remarks on topic etc.

## Textual syntax

Sentences are built from _simple sentential phrases_ using _sentential connectives_.

In Naproche, there are four kinds of some simple sentential phrases.

* Formulae: given by TeX expressions.
* NP-VP sentences
* Metasentences
* Quantified sentences

The main case is _NP-VP sentences_. The higher level structure, such as _assertions_ and _assumptions_, comes from sentence connectives.

### Noun phrases

These are either formulae or _determiner noun phrases_, with the latter the main case. Examples from the thesis are:

1. an integer
2. an even integer $k$.
3. no finite sets
4. the set of odd prime numbers
5. some even numbers $a_x$ , $b_x$ satisfying the following properties.
6. every circle $C$ such that $p$ lies on $C$.
7. distinct points $p_1$ , $p_2$ and $p_3$ on $L$ such that $d(p_1 , p_2 ) \leq d(p_2 , p_3)$ or
$d(p_1 , p_2 ) > 1$.
8. points not on $L$.
9. no $k$ such that $k^2 > n$.

A determiner noun phrase has:

* A determiner (possibly the _zero determiner_)
* An optional list of adjectives
* the core: a noun, a list of quantterms, or a noun followed by a list of quantterms.
* a Post-modifier

On parsing, one automatically gets more general post-modifiers than in Naproche, so fewer cases, namely:

* Prepositional phrases: these are prepositions followed by noun phrases
* such that clauses
* satisfying phrases - 'which statisfy the following:'

**Note:** A noun or verb can have more than one word.

### Verb phrases

A verb phrase is one of

* an affirmative verb phrase
* the negation of an affirmative verb phrase

An affirmative verb phrase is one of:
* an intransitive verb
* a transitive verb with its object
* A copula followed by one of
  * a noun phrase
  * an intransitive adjective
  * a transitive adjective (e.g., coprime)
  * a transitive adjective followed by a prepositional phrase.
  * a such-that clause
  * a prepositional phrase

## Additional usage

* without loss of generality
* the following are equivalent
