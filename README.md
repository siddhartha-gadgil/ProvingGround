# ProvingGround
Proving Ground: Tools for Automated Mathematics

A system under development for (semi-)automated theorem proving, with foundations *homotopy type theory*, using
*machine learning*, both by _reinforcement learing_ using backward-propagation and using natural language processing to assimilate part of the mathematics literature.

[![Build Status](https://img.shields.io/travis/siddhartha-gadgil/ProvingGround.svg)](https://travis-ci.org/siddhartha-gadgil/ProvingGround)
[![codecov.io](http://codecov.io/github/siddhartha-gadgil/ProvingGround/coverage.svg)](https://codecov.io/gh/siddhartha-gadgil/ProvingGround)
[![Ohloh](http://www.ohloh.net/p/ProvingGround/widgets/project_thin_badge.gif)](https://www.ohloh.net/p/ProvingGround)


## Documentation:

* The main documentation is on the [website](http://siddhartha-gadgil.github.io/ProvingGround/) , including [scaladocs](http://siddhartha-gadgil.github.io/ProvingGround/scaladoc/provingground/index.html).
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
