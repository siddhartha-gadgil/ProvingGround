As an alternative to the repl with some conveniences (and inconveniences), there is a crude experimental code scripting server, which can be downloaded as a [zip file](./provingground-server-0.1.zip) or a [zipped tarball](./provingground-server-0.1.tgz). To run a local server extract and, run the script or `.bat` script(windows) in the `bin` folder. This requires Java 8 to be installed.


This can also be started with

```scala
sbt server/run
```

or better still
```scala
sbt
~server/run
```

This gives an interface to an ammonite kernel with the core of the provingground-core in the classpath, and some imports to start. In this, one can:

* run code and see the result/error (there is no response if the code has only imports)
* save a script (at present to a default folder only).
* load a previously saved script
* create an object in the `provingground.scripts` package in the core.
* insert a script made into an object (at the cursor)

After creating an object one can restart the server (if started with `~server/run`, simply exit) and use this in another script, a REPL session or the main code. The scripts can also be imported for use in ammonite as they are valid ammonite scripts as long as the core project is in the classpath.

Alternatively, a script wrapped into an object can be inserted into the editor to be used in another script.

In the longer run
* this can be packaged, so a user needs just a jar (or even native packaging).
* this will be part of a server helping with other aspects of the code.
