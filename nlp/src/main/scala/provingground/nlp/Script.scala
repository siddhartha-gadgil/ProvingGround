package provingground.translation
import provingground._

import ammonite.ops._

import edu.stanford.nlp.simple._

object Script {
  val file = pwd / "notes" / "NLPtoHoTT.markdown"

  def save(s: String) = write.append(file, "\n" + s + "\n")

  def saveCode(s: String) = save(s"""```\n$s\n```""")

  def saveQuote(s: String) = save(s"""* ``$s``""")

  def parse(s: String) = {
    val sent = new Sentence(s)
    val tree = sent.parse
    tree.pennPrint
    saveQuote(sent.toString)
    saveCode(tree.pennString)
    tree
  }
}
