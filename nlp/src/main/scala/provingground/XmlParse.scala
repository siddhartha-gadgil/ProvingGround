package provingground

import translation._

import NlpProse._

@deprecated("0.1", "call parser directly")
object XmlParse {

  /** Load XML file */
  def xmlSrc(filename: String) = xml.XML.loadFile(filename)

  private def cc(parse: scala.xml.NodeSeq) =
    parse \\ "collapsed-ccprocessed-dependencies"

  private def deps(parse: scala.xml.NodeSeq) = cc(parse) \ "dep"

  /** Extract Token from XML */
  def xmltoToken(node: scala.xml.Node) =
    Token(node.text, (node \ "@idx").head.text.toInt)

  /** Extract Dependency relation from XML */
  def xmltoDepRel(node: scala.xml.Node) =
    DepRel(xmltoToken((node \ "governor").head), xmltoToken((node \ "dependent").head), (node \ "@type").head.text)

  /** Extract List of Dependency relations from XML Source */
  def depTree(parse: scala.xml.NodeSeq) =
    (deps(parse) map (xmltoDepRel)).toList

  /** Extract ProseTree from XML file folding in multi-word expressions */
  def proseTree(filename: String) =
    mweFold(ProseTree(depTree(xmlSrc(filename))))
}
