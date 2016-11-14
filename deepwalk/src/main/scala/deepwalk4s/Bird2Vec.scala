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

object Bird2Vec{
  val data = cwd / "data"

  val checklists =
    read.lines(data / "checklists.tsv") map (_.split("\t").toVector.tail)

  val rnd = new scala.util.Random()

  def randSentence(length: Int = 10) = {
    val vec = checklists(rnd.nextInt(checklists.size))
    (1 to length).toVector map {(_) => vec(rnd.nextInt(vec.size))}
  }

  val bs = data / "bird-sentences.txt"

  def writeSentences(numSentences: Int = 10000000, length: Int = 10) =
    (1 to numSentences).foreach{(_) =>
      write.append(bs, randSentence(length).map(_.replace(" ", "@")).mkString("", " ", "\n"))
    }

  lazy val iter = new BasicLineIterator(new File("/home/gadgil/code/ProvingGround/data/bird-sentences.txt"))

  lazy val t = new DefaultTokenizerFactory();
        t.setTokenPreProcessor(new CommonPreprocessor());

  def word2vec(dim: Int = 15, loops: Int = 1) : Word2Vec = {
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

  def save(vec: Word2Vec)=
    WordVectorSerializer.writeWordVectors(vec, new File("/home/gadgil/code/ProvingGround/data/bird2vec.txt"))



}
