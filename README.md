# ProvingGround
Proving Ground: Tools for Automated Mathematics

A system under development for (semi-)automated theorem proving, with foundations *homotopy type theory*, using
*machine learning*, both by _reinforcement learing_ using backward-propagation and using natural language processing to assimilate part of the mathematics literature.

[![Build Status](https://img.shields.io/travis/siddhartha-gadgil/ProvingGround.svg)](https://travis-ci.org/siddhartha-gadgil/ProvingGround)
[![codecov.io](http://codecov.io/github/siddhartha-gadgil/ProvingGround/coverage.svg)](https://codecov.io/gh/siddhartha-gadgil/ProvingGround)
[![Ohloh](http://www.ohloh.net/p/ProvingGround/widgets/project_thin_badge.gif)](https://www.ohloh.net/p/ProvingGround)

## Implemented:

* Most of Homotopy type theory.
* Symbolic Algebra integrated with HoTT.
* Importing from the lean theorem prover.
* Some non-trivial proofs as examples.
* Most of the dynamics for learning with homotopy type theory.
* Basic case for goal-driven discovery
* An akka-http server, and integration with scala-js
* Sketch for using stanford-corenlp tools to map to an intermediate (Naproche inspired) language.

## Documentation:

There is not much besides the source.

* The [website](http://siddhartha-gadgil.github.io/ProvingGround/) has the most current documentation, including [http://siddhartha-gadgil.github.io/ProvingGround/unidoc/provingground/index.html](scaladocs).
* The [notes](https://github.com/siddhartha-gadgil/ProvingGround/tree/master/notes) folder contains Jupyter notebooks illustrating some of the code.
* Some documentation is in the [project wiki](https://github.com/siddhartha-gadgil/ProvingGround/wiki).

## Contributors

This project has _greatly_ benefited by contributions from

* Dymtro Mitin
* Tomoaki Hashizaki
* Olivier Roland
* Ankit Jaiswal
* Sayantan Khan

The principal developer is Siddhartha Gadgil (Department of Mathematics, Indian Institute of Science, Bangalore).

## Running

At present the main way to run the code is to load a console (for an alternative, visit the [website](http://siddhartha-gadgil.github.io/ProvingGround/)). For example, in the home of the project, run
```
sbt mantle/run
```
to pop up a nice console (Li Haoyi's ammonite repl), with many imports already in scope.
