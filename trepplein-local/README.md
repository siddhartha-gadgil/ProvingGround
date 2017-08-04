## trepplein: a Lean type-checker

Lean is an interactive theorem prover based on dependent type theory.  For
additional trust, Lean can [export the generated proofs][1] so that they can be
independently verified.  Trepplein is a tool that can check these exported proofs.

[1]: https://github.com/leanprover/lean/blob/master/doc/export_format.md

Trepplein is written in Scala, and requires [SBT](http://www.scala-sbt.org/) to
build.
```
sbt stage
./target/universal/stage/bin/trepplein .../export.out
```

### Other checkers

* [tc](https://github.com/dselsam/tc), a type-checker written in Haskell.
* [leanchecker](https://github.com/leanprover/lean/tree/master/src/checker), a bare-bones version of the Lean kernel.
