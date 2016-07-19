package provingground

// import scala.swing._
import java.awt.Dimension
import akka.actor._

import provingground.TextToInt._

object AckGUI /*extends SimpleSwingApplication*/ {
  /*
    val s = new Dimension(1500, 1500)

	object Ack{
		val posInts: Stream[Int] = Stream.from(0)
		def ack(m: BigInt, n: BigInt): BigInt = {
			  if (m==0) n+1
			  else if (n==0) ack(m-1, 1)
			  else ack(m-1, ack(m, n-1))
			}
		def ackStreamM(m: Int): Stream[BigInt] = for (n <- posInts) yield ack(m,n)
		val ackStream = for (m<- posInts) yield ackStreamM(m)
		def apply(m: Int, n:Int) = ackStream(m)(n)
		}

	def ack(n: Int) = n * n

	val toCompute = new TextArea(10, 10){
			charWrap = true
			}
	val computeButton = new Button{
		text = "Compute"
		verticalAlignment = Alignment.Top
		}
	val computeFrame = new FlowPanel{
	    contents += computeButton
	    contents += toCompute
	    border = Swing.EmptyBorder(20, 20, 20, 20)
	}

	val computeResult = new TextArea(10, 40){
			charWrap = true
			}

	val leftPanel = new BoxPanel(Orientation.Vertical){
			contents += new Label("Enter number to compute Ackerman function")
			contents += computeFrame
			contents += new Label("Value")
			contents += computeResult
			border = Swing.EmptyBorder(20, 20, 20, 20)
			}




	def top = new MainFrame{
		title = "Computing the Ackermann function A(m, n) for n=2"
		contents = new BoxPanel(Orientation.Horizontal){
		    contents += leftPanel
				minimumSize = s
		}
		}
	listenTo(computeButton)

	reactions +={
		case swing.event.ButtonClicked(`computeButton`) =>
			computeResult.text = toCompute.text match {
				case Int(m) if m>=0 => Ack.ack(m, 2).toString
				case _ => "Ackerman function is defined only for integers"
				}
		}*/
}
