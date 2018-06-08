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

#### Servers

Two rudimentary servers are available as binaries, which you can download and run. You need Java 8 installed. In Unix systems you may need to run `chmod +x ...` to make the files executable.

* [ProvingGround HoTT Server](http://math.iisc.ac.in/~gadgil/proving-ground/bin/provinground-mantle-SNAPSHOT)
* [ProvingGround NLP Server](http://math.iisc.ac.in/~gadgil/proving-ground/bin/provinground-nlp-SNAPSHOT)

Start one of these servers and visit `localhost:8080` on a browser to run. You can also specify the port by starting with a `-p` option.

These will be frequently updated with new features.

#### From Source

At present the best way to interact with most of the code is to use a console in either [mill](https://www.lihaoyi.com/mill/) or `sbt` (the primary build tool is now [mill](https://www.lihaoyi.com/mill/)). To pop up a console with most of the code in scope, install [mill](https://www.lihaoyi.com/mill/) and run:
```
mill mantle.repl
```

for the HoTT implementation etc, or

```
mill nlp.repl
```
for the natural language processing part.

To experiment with _natural language processing_, a basic server can be started by running
```
mill nlp.run
```
and going to `localhost:8080` on the browser. To experiment with the code, you can use the `--watch` flag so the system restarts after shutting down from the browser.

Similarly, one can experiment with a small part of the HoTT implementation by running
```
mill mantle.run
```
