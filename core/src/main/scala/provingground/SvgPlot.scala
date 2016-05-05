package provingground
import scala.xml._
import scala.language.implicitConversions

/**
 * @author gadgil
 */
class SvgPlot(width: Int, height: Int, scale: Double =1.0) {
  val xmax = width/2.0
  val ymax = height/2.0

  def xval(x: Double) = ((x * scale) + xmax).toInt

  def yval(y: Double) = ( ymax -(y * scale)).toInt

  def line(x1 : Double, y1: Double, x2: Double, y2: Double,
      colour: String ="blue") = {
    <line x1={xval(x1).toString}
		y1={yval(y1).toString} x2={xval(x2).toString} y2={yval(y2).toString} stroke={colour} stroke-width="2" />
  }

  def polyline(pts: (Double, Double)*) = {
    val stl = pts map {case (x, y) => s"${xval(x)},${yval(y)}"}
    val ptsStr = stl mkString(" ")
    <polyline points={ptsStr}/>
  }

  def graph(points : Seq[(Double, Double)], colour: String ="blue") = {
    val pairs = points zip (points.tail)
    pairs map {case ((x1, y1), (x2, y2)) => line(x1, y1, x2, y2, colour)}
  }

  def circle(x1 : Double, y1: Double, r: Double,
      colour: String ="blue") = {
    <circle cx={xval(x1).toString}
      cy={yval(y1).toString} r={r.toString}
      stroke={colour} stroke-width="2" />
  }

  val axes = List(line(-xmax, 0, xmax, 0, "blue"),
      line(0, -ymax, 0, ymax, "blue")
      )

  def plot(nodes: Seq[Node]) = <svg width={width.toString} height={height.toString}>{NodeSeq fromSeq nodes}</svg>

  def plotAll(nodes: Node*) = <svg width={width.toString} height={height.toString}>{NodeSeq fromSeq (nodes.toSeq)}</svg>

  import SvgPlot._

  def draw(l: Line) = line(l.start.x, l.start.y, l.end.x, l.end.y, l.colour)

  def draw(c: Circle) = circle(c.centre.x, c.centre.y, c.radius, c.colour)

  def scatter(points: List[Point], radius: Double = 5): Seq[Node] = {
    points map ((point) =>
      <circle x1={xval(point.x).toString}
      y1={yval(point.y).toString} r={radius.toString}
      stroke={point.colour} stroke-width="2" fill={point.colour}/>)
  }
}

object SvgPlot{
  case class Point(x: Double, y: Double, colour: String="blue"){
    def ->:(that: Point) = Line(that, this, colour)
  }

  case class Circle(centre: Point, radius: Double, colour: String="red")

  case class Line(start: Point, end: Point, colour: String="blue")

  implicit def pairPoint(ab: (Double, Double)) = Point(ab._1, ab._2)
}
