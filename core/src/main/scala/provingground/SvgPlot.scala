package provingground
import scala.xml._
import scala.language.implicitConversions

/**
 * @author gadgil
 */
class SvgPlot(width: Int, height: Int, scale: Double =1.0) {
  val xmax = width/2
  val ymax = height/2
  
  def xval(x: Double) = ((x * scale) + xmax).toInt
  
  def yval(y: Double) = ( ymax -(y * scale)).toInt
  
  def line(x1 : Double, y1: Double, x2: Double, y2: Double, 
      colour: String ="blue") = {
    <line x1={xval(x1).toString}  
		y1={yval(y1).toString} x2={xval(x2).toString} y2={yval(y2).toString} stroke={colour} stroke-width="2" />
  }
  
  def circle(x1 : Double, y1: Double, r: Double, 
      colour: String ="blue") = {
    <circle x1={xval(x1).toString} 
      y1={yval(y1).toString} r={r.toString} 
      stroke={colour} stroke-width="2" />
  }
  
  val axes = List(line(-xmax, xmax, 0, 0, "blue"),
      line(0, 0, -ymax, ymax, "blue")
      )
  
  def plot(nodes: Seq[Node]) = <svg width={width.toString} height={height.toString}>{NodeSeq fromSeq nodes}</svg>
  
  import SvgPlot._
  
  def draw(l: Line) = line(l.start.x, l.start.y, l.end.x, l.end.y, l.colour)
  
  def draw(c: Circle) = circle(c.centre.x, c.centre.y, c.radius, c.colour)
}

object SvgPlot{
  case class Point(x: Double, y: Double, colour: String="blue"){
    def ->:(that: Point) = Line(that, this, colour)
  }

  case class Circle(centre: Point, radius: Double, colour: String="red")
  
  case class Line(start: Point, end: Point, colour: String="blue")
  
  implicit def pairPoint(ab: (Double, Double)) = Point(ab._1, ab._2)
}