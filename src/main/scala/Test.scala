package provingGround

import provingGround.XmlParse._
import provingGround.ParseProse._

/** Parses the sentence in a given xml file */
object Test extends App{
  private val filename = if (args.isEmpty) "example.xml" else args(0)
	private val tree = proseTree(filename)
	println(tree)
	val formula = toFormula(tree, Global)
	println(formula)
	println(formula.freeVars)
	}