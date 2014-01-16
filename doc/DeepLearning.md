# Learning Deep/Representation learning

## The problem:

### Context: 

* We are studying a collection X of complex objects: natural images, sentences, mathematics, software code, tennis strokes etc.

* These are embedded in a much larger collection of raw objects: sequences of words, bitmaps etc., forming an unknown huge subset, but only a tiny fraction of all raw data.

* We have an objective, say associate to each x in X a value y in Y (i.e., have a function f: X -> Y), where we have a measure of how good y is for given x.

So far this looks like a fairly conventional statistics problem. The difference lies in the nature of data

### Data:

The data based on which we work is: 

* A relatively small amount of _annotated_ data: either giving the correct y for some x or at least saying how good a given y is for some x.

* An enormously larger collection of objects which we know lie in X, but nothing more.

The fundamental problem is to make good use of the second kind of data.  Using the first kind is conventional statistics.

* Sometimes we simply start with data of the second kind and prepare ourselves for applications when some annotated data is given later.

### Representation learning

* Essentially, to understand X we want to find ways of representing, perhaps with a loss of information, objects in x in a more effecient way than with raw data.

* Fourier transforms, wavelets etc. give efficient linear representations.

* Most complex objects are, however, modular in nature, with layers of abstractions. This is most obvious in the case of code in a high level language. This ultimately compiles to a huge number of instructions running on a universal Turing machine, but in practise we build layers of abstraction on the basic commands: Loops and Conditionals, Functions, Objects, Collections, Event Streams etc. so that the code required is relatively small.

* Similarly, proofs consist of lemmas, functions and constructions are built from others by composition etc.

* The building blocks combine _non-linearly_ - as non-linear tends to be viewed somewhat mystically, I find it helpful to keep in mind a couple of non-linear combinations: _conditionals_ (if (x>0) x else -x)  and _compositions_ of functions.

### Deep learning

* We postulate that the data we have, or at least many aspects of it, are essentially modular: natural images are made of trees, stones etc., trees are made of branches, leaves, flowers etc.

* We are given a large collection of the composite objects, perhaps along with collections of components.

* We wish to read the modular nature  of the data using the _unannoted_ objects.

* This is done by using a layered approach where:

* The first layer finds components that efficiently describe the objects in X, with the components allowed to combine non-linealry.

* Now instead of the raw data, consider the given objects in X as described by those in the first layer. Thus our raw data is now a collection of combinations of, say, lines and corners, instead of 0's and 1's.

* We can repeat the process, and build a second layer of components chosen to describe the data given in terms of first-layer components. This should capture the next level of abstraction.

* On repeating this a few times, completely raw data should be represented in terms of layers of abstraction, capturing the modular structure.

* One can also have feedback in the other direction: the objects of the first layer can be changed to not only fit the original data but also the second layer. I do not know how much difference this makes in practise.

### 10,000 hours

* It is often said that experitse needs 10,000 hours of purposive learning. Much of this may be building the layers of abstraction.

* (following Marvin Minsky) As software (including its tuning) can just be copied, we can become experts instead in just a few hours (just learn to communicate with software).



