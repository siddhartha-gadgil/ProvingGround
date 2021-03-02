package provingground.andrewscurtis

import ammonite.ops._

/**
  * Batch start for Andrews-Curtis runs
  */
object ACBatch {
  val wd = pwd / "data"

  /**
    * start data before json parsing
    */
  def loadRawStartData(dir: String = "acDev", file: String = "acbatch.json") = {
    val jsFile = if (file.endsWith(".json")) file else file + ".json"
    val js =
      ammonite.ops.read.lines(wd / dir / jsFile) filter
        ((l) => !(l.startsWith("#")))
    println(js)
    js
  }

  /**
    * start data from file.
    */
  def loadStartData(dir: String = "acDev", file: String = "acbatch.json") = {
    val d = loadRawStartData(dir, file)
    d map (StartData.fromJson)
  }

  /**
    * Load start data, start actors, return references to these.
    */
  def quickStart(dir: String = "acDev", file: String = "acbatch.json") = {
    val ds      = loadStartData(dir, file)
    val loopers = ds map (_.run())
    loopers
  }
}
