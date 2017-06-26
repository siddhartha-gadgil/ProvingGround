# Natural language mathematics based on Naproche

Our goal is to translate human mathematical literature into semanticaly represented mathematics, eventually in HoTT, for the sake of _learning_, hence allowing some error and multiple possible translations. Here we summarize many components of the language of mathematics as found in the wild, based on both the Naproche CNL and the concluding section of Cramer's thesis about missing idioms.

In addition to this, real life mathematics concerns _attention_ and _relations_.

### Quantterms and pronouns

Quantterms are $n$, $n_k$ etc, and function like pronouns. In addition, 'it' and 'they' are used.

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

**Determiners**: 
Besides the standard English ones, we (should) have 
* cardinals, 
* 'precisely one of' etc. 
* even allow eg _at least two_
* _such a_, eg _such a function_

**Transitive nouns**: Unlike Naproche, a transitive noun with a prepositional phrase is parsed.

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
* (extending Naproche) inflected forms of _to have_, _with_ and _without_, eg _a Ring without zero divisors_

While parsing, copula, have etc are picked up as verbs with objects, with special rules at interpretation stage.

### Metalingual NP phrases

These may be parsed as ordinary NPs.

#### Anaphoric

Refers to previously defined cases, properties, axioms etc, and combines by conjunction, disjunction etc. 

In natural language, also have other references to earlier definitions, eg
* _it_ and _they_
* anaphoric definite noun phrases, eg _the group_ for a previously definied group.

#### Cataphoric NPs

Example: _One of the following cases_ followed by a list of statements.

### Metalingual VP phrases

Inflected form of _to hold_, _to be true_, _to be correct_, _to be incorrect_, _to be false_, _not to be true_, _not to be correct_, _not to hold_ or _to be inconsistent_. In HoTT can even use propositions as types to make these mathematical.

## Quantified sentences

Existentially or universally quantified sentences.

## Sentential Connectives

Some of these are special: _assertion triggers_, _assumption triggers_ and _justifications_ (by reference).

* Assertion triggers include: _thus_,_also_, _and_, _but_, _clearly_, _finally_, _furthermore_, _hence_, _i.e._, _in particular_, _now_, _observe that_, _obviously_, _recall that_, _so_, _therefore_, _this (in turn) implies (that)_,  _trivially_; and the _empty assertion trigger_.
* Assumption triggers include: _(now) assume (that)_, _(now) assume for a contradiction that_, _(now) suppose that_, _(now) let_ and _(now) consider_.
* Justifications are prepositional phrases _by Theorem-n_, _using ..._ etc. In general statements should be allowed here, which means we have two assertions.
* Assumptions can also be formed by _consider_, _fix_, _let ... be given_, _take_ etc; these are really assertion-assumptions

We also have the simpler sentential connectives, the result of which form expressions: _if_, _and_, _or_, _i.e._, _if and only if_, _is (not) the case that_ 


## Additional usage (most from Naproche thesis).

* _without loss of generality_ (also _say_ used similarly): a special _justification_.
* _the following are equivalent_: another case of cataphoric metaphrases (like _all of the following_).
* ellipsis 
* reduction of statements, 'we still need to show that' etc
* the word respectively
* many forms of definitions:
  * _we say that_
  * _is called_
  * restricting previous definitions.
* flexible conclusion by contradiction.
* which and that clauses - at present this is crudely handled by rewriting as _where it_.
* _otherwise_ to negate assumptions
* _by definition of ..._ as justification; also _by assumption_ and equivalents such as _since we consider ..._
* _in addition_, _as well_
* _is as required_, _fulfills the requirements_, _as claimed_
* eg _if we count ..._ : there are relatively few actions that 'we' do (see active verbs below).
* replacing one object by another in a formula/expression.
* abuse of notation, abbreviations eg 'for short'
* Example: "On its domain, the discrete one-forms dz and dz are defined in such a way that e dz = z(e) and edz = z(e) hold for any oriented edge e of X" : here the justification is the main part of the sentence.
* Have a primitive notion of functions, and hence of being dependent on something, defined somewhere etc
* Meta-terms include _natural_, _canonical_
* 'associated to', 'gives rise to'
* **Equality** (the word 'same')
* 'in this sense'
* forget, ignore


### Active verbs

* Besides verbs like _prove_ and _see_ with 'we' as a subject, we _identify_, _glue_, _push_, _drag_, _homotop_ 
* geometric proofs are those with plenty of such active verbs
* the harder case is when these are parts of _definitions_, not just _justifications_.
* many like _identify_ and _glue_ do have precise meanings.
