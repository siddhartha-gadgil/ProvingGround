package compact_enumeration
import scala.xml._

/**
 * @author gadgil
 */
class SvgPlot(width: Int, height: Int, scale: Double =1.0) {
  val xmax = width/2
  val ymax = height/2
  
  def xval(x: Double) = ((x * scale) + xmax).toInt
  
  def yval(y: Double) = ( ymax -(y * scale)).toInt
  
  def drawLine(x1 : Double, y1: Double, x2: Double, y2: Double, 
      colour: String ="orange") = {
    <line x1={xval(x1).toString} x2={xval(x2).toString} 
			y1={yval(y1).toString} y2={yval(y1).toString}
      stroke={colour}/>
  }
  
  val axes = List(drawLine(-xmax, xmax, 0, 0, "blue"),
      drawLine(0, 0, -ymax, ymax, "blue")
      )
  
  def plot(nodes: Seq[Node]) = <svg>{NodeSeq fromSeq nodes}</svg>
}