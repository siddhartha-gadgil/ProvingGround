package deepwalk4s

import ammonite.ops._

// import org.deeplearning4j.models.embeddings.inmemory.InMemoryLookupTable
// import scala.collection.mutable
import org.deeplearning4j.models.word2vec.Word2Vec
import org.deeplearning4j.text.tokenization.tokenizerfactory.DefaultTokenizerFactory
// import org.deeplearning4j.plot.BarnesHutTsne
import org.deeplearning4j.models.embeddings.loader.WordVectorSerializer
import org.deeplearning4j.text.sentenceiterator.BasicLineIterator
import java.io.File
import org.deeplearning4j.text.tokenization.tokenizer.preprocessor._

object Bird2Vec {
  val data = cwd / "data"

  val freqF = data / "frequencies.tsv"

  val birdFreqs = read.lines(freqF).map(_.split("\t")).map {
    case Array(name, n) => (name, n.toInt)
  }

  val commonBirds = { for ((name, f) <- birdFreqs if f > 100) yield name }.toSet

  val checklists =
    read.lines(data / "checklists.tsv") map
      (_.split("\t").toVector.tail.filter(commonBirds.contains(_)))

  lazy val commonPairs = {
    for (x <- commonBirds; y <- commonBirds if x != y) yield (x, y)
  }.toVector

  val commonNames = read
    .lines(data / "common-names.tsv")
    .map(_.split("\t"))
    .map { case Array(sci, comm) => (sci, comm) }
    .toMap

  val rnd = new scala.util.Random()

  def randSentence(length: Int = 10) = {
    val vec = checklists(rnd.nextInt(checklists.size))
    (1 to length).toVector map { (_) =>
      vec(rnd.nextInt(vec.size))
    }
  }

  val bs = data / "bird-sentences.txt"

  def writeSentences(numSentences: Int = 10000000, length: Int = 10) =
    (1 to numSentences).foreach { (_) =>
      write.append(bs,
                   randSentence(length)
                     .map(_.replace(" ", "@"))
                     .mkString("", " ", " . \n"))
    }

  lazy val iter = new BasicLineIterator(
    new File("/home/gadgil/code/ProvingGround/data/bird-sentences.txt"))

  lazy val t = new DefaultTokenizerFactory();
  t.setTokenPreProcessor(new CommonPreprocessor());

  def word2vec(dim: Int = 15, loops: Int = 1): Word2Vec = {
    val vec = new Word2Vec.Builder()
      .minWordFrequency(5)
      .iterations(loops)
      .layerSize(dim)
      .windowSize(5)
      .iterate(iter)
      .tokenizerFactory(t)
      .build();
    vec.fit()
    vec
  }

  def save(vec: Word2Vec) =
    WordVectorSerializer.writeWordVectors(
      vec,
      new File("/home/gadgil/code/ProvingGround/data/bird2vec.txt"))

  def nearest(vec: Word2Vec) =
    commonPairs
      .sortBy((ab) =>
        -vec.similarity(ab._1.replace(" ", "@"), ab._2.replace(" ", "@")))
      .map { case (a, b) => (commonNames(a), commonNames(b)) }
}
