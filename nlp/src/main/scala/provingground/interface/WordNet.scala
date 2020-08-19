package provingground.interface

import provingground._

import edu.mit.jwi._, item._

import scala.jdk.CollectionConverters._

import java.io.File

import scala.util._

object WordNet{
  val url = getClass().getClassLoader().getResource("dict")

  val urlDirect = new File("nlp/data/dict").toURI().toURL()

  os.makeDir.all(os.pwd / "tmp" / "dict")

  // os.copy.over((os.resource / "dict", os.pwd / "tmp" / "dict", createFolders = true)

  pprint.log(url)

  val dict = if (url.toString().startsWith("file:")) new Dictionary(url)
    else new Dictionary(urlDirect)

  dict.open()

  def getWordIdsPos(word: String, pos: POS) : Vector[IWordID] = {
    Option(
      dict.getIndexWord(word, pos))
      .toVector
      .flatMap(
        _.getWordIDs().asScala.toVector
      )
  }

  def synSet(word: IWordID) =
    dict.getSynset(word.getSynsetID)

  val noun = POS.NOUN

  val verb = POS.VERB

  val adj = POS.ADJECTIVE

  val adv = POS.ADVERB

  val pennPos = Map(
    noun -> "NN",
    verb -> "VB",
    adj -> "JJ",
    adv -> "RB"
  )

  def getWordIDsMap(word: String) =
    for {
      (pos, s) <- pennPos
    } yield s -> getWordIdsPos(word, pos)

  def posTags(word: String) =
    getWordIDsMap(word).filter(_._2.nonEmpty).keys.toVector
}
