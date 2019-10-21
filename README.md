# ProvingGround

Proving Ground: Tools for Automated Mathematics

A system under development for (semi-)automated theorem proving, with foundations *homotopy type theory*, using
*machine learning*, both by _reinforcement learing_ using backward-propagation and using natural language processing to assimilate part of the mathematics literature.

[![Build Status](https://img.shields.io/travis/siddhartha-gadgil/ProvingGround.svg)](https://travis-ci.org/siddhartha-gadgil/ProvingGround)
[![codecov.io](http://codecov.io/github/siddhartha-gadgil/ProvingGround/coverage.svg)](https://codecov.io/gh/siddhartha-gadgil/ProvingGround)
[![Ohloh](http://www.ohloh.net/p/ProvingGround/widgets/project_thin_badge.gif)](https://www.ohloh.net/p/ProvingGround) [![Join the chat at https://gitter.im/siddhartha-gadgil/ProvingGround](https://badges.gitter.im/siddhartha-gadgil/ProvingGround.svg)](https://gitter.im/siddhartha-gadgil/ProvingGround?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
![](https://github.com/siddhartha-gadgil/ProvingGround/workflows/scala%20CI/badge.svg)

## Documentation

* The main documentation is on the [website](http://siddhartha-gadgil.github.io/ProvingGround/) , including [scaladocs](http://siddhartha-gadgil.github.io/ProvingGround/scaladoc/provingground/index.html). The same site also hosts [working notes](https://siddhartha-gadgil.github.io/ProvingGround/notes/).
* The [notes](https://github.com/siddhartha-gadgil/ProvingGround/tree/master/notes) folder contains Jupyter notebooks illustrating some of the code.
* Some documentation is in the [project wiki](https://github.com/siddhartha-gadgil/ProvingGround/wiki).

## Try it with zero installation

You can try the project with zero installation on _scastie_, for example the [HoTT worksheet](https://scastie.scala-lang.org/siddhartha-gadgil/0DqN82WeQk2W0nqSYQpolA).
If you want to try your own worksheet add the library `provvingground-core-jvm` (which can be found with scastie's search). More worksheets and info will be posted soon.

## Contributors

This project has _greatly_ benefited by contributions from

* Dymtro Mitin
* Tomoaki Hashizaki
* Olivier Roland
* Sayantan Khan

The principal developer is [Siddhartha Gadgil](http://math.iisc.ac.in/~gadgil) (Department of Mathematics, Indian Institute of Science, Bangalore).

## Running

### Servers

Two rudimentary servers are available as binaries, which you can download and run. You need Java 8 installed. In Unix systems you may need to run `chmod +x ...` to make the files executable.

* [ProvingGround HoTT Server](http://math.iisc.ac.in/~gadgil/proving-ground/bin/provinground-mantle-SNAPSHOT)
* [ProvingGround NLP Server](http://math.iisc.ac.in/~gadgil/proving-ground/bin/provinground-nlp-SNAPSHOT)

Start one of these servers and visit `localhost:8080` on a browser to run. You can also specify the port by starting with a `-p` option (and interface using `-i`).
Note that the second server also includes most of the first server.

These will be frequently updated with new features.

### From Source

At present the best way to interact with most of the code is to use a console in either [mill](https://www.lihaoyi.com/mill/) or `sbt` (the primary build tool is now [mill](https://www.lihaoyi.com/mill/)).  
Note that _trepplein_ is a git submodule and is a dependency of part of the code, so you will have to [clone submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules#_cloning_submodules).  
The simpliest way to do this is to clone the project with submodules :

```bash
git clone --recurse-submodules --single-branch https://github.com/siddhartha-gadgil/ProvingGround.git
```

or if you have already clone the project without submodules, you can fetch them afterwards :

```bash
git submodule update --init
```

To pop up a console with the core code in scope, run (you need Java 8 installed, but mill need not be installed as it has a bootstrap script):

```bash
./mill -i core.jvm.repl
```

For running with some IO code (e.g. parsing lean exports), instead run

```bash
./mill -i mantle.repl
```

and

```bash
./mill -i nlp.repl
```

for the natural language processing part.

To experiment with _natural language processing_, a basic server can be started by running

```bash
./mill nlp.run
```

and going to `localhost:8080` on the browser. To experiment with the code, you can use the `--watch` flag so the system restarts after shutting down from the browser.

Similarly, one can experiment with a small part of the HoTT implementation by running

```bash
./mill mantle.run
```

### Using a Notebook interface

A useful way to experiment is to use a _notebook_ instead of a repl session to ensure persistence. To do this:

* Install [Jupyter](https://jupyter.org/) and the [almond kernel](https://almond.sh/)
* Generate a binary in the `notes/bin` folder. The first time you do this, you need to run `mkdir notes/bin` in the shell first.

```bash
./mill core.jvm.bin
```

This generates the binary and gives the command to use in _jupyter-lab_ or the classic jupyter notebook. Note that you must launch jupyter-lab in the notes folder.

