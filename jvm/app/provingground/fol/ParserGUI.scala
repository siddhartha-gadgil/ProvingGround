package provingground.fol

//import scala.swing._
import java.awt.Dimension

import provingground.CoreNLP._
import provingground.fol.Logic._
import provingground.NlpProse._
import provingground.fol.ParseProse._


 object ParserGUI /*extends SimpleSwingApplication*/{/*
    val s = new Dimension(1500, 1500)
    val pipe = newPipe
	val toParse = new TextArea(10, 40){
			charWrap = true
			}
	val parseButton = new Button{
		text = "Parse"
		verticalAlignment = Alignment.Top
		}
	val parseFrame = new FlowPanel{
	    contents += parseButton
	    contents += toParse
	    border = Swing.EmptyBorder(20, 20, 20, 20)
	}

	val parseResult = new TextArea(10, 40){
			charWrap = true
			}

	val leftPanel = new BoxPanel(Orientation.Vertical){
			contents += new Label("Enter text to parse to a Formula")
			contents += parseFrame
			contents += new Label("Formula obtained")
			contents += parseResult
			border = Swing.EmptyBorder(20, 20, 20, 20)
			}

	var depListView = new ListView(List():List[DepRel])

	var depHeader = new Label("        ")

	val rightPanel = new BoxPanel(Orientation.Vertical){
					contents += new Label("Root Token")
	        contents += depHeader
					contents += new Label("Typed Dependencies")
	        contents += new ScrollPane(depListView)
	}

	def top = new MainFrame{
		title = "Parsing Prose into Logic"
		contents = new BoxPanel(Orientation.Horizontal){
		    contents += leftPanel
		    contents += rightPanel
				minimumSize = s
		}
		}
	listenTo(parseButton)

	reactions +={
		case swing.event.ButtonClicked(`parseButton`) =>
		  val parsedTree =proseTrees(toParse.text, pipe).head
			parseResult.text = toFormula(parsedTree, Global).toString
			depListView.listData = parsedTree.tree
			depHeader.text = parsedTree.root.toString
		}*/
	}
