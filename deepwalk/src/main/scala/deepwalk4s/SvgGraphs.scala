package deepwalk4s

object SvgGraphs {
  val labelCss = """
    <style>
      .labl {
        display: none;
      }
      .labelled:hover + .labl {
        display: inline;
      }
    </style>
    """

  def circ(x: Double,
           y: Double,
           t: String,
           r: Double = 3,
           colour: String = "blue",
           fontSize: Int = 12) = {
    s"""
    <circle cx="$x" cy="$y" r="$r" fill="$colour" class = "labelled"/>
    <text x="${x + r +
      r}" y="$y" font-size="$fontSize" text-anchor="start" class = "labl">$t</text>
    """
  }

  def header(width: Int = 1000, height: Int = 400) = {
    s"""
    $labelCss

    <svg version="1.1"
   baseProfile="full"
   width="$width" height="$height"
   xmlns="http://www.w3.org/2000/svg">

   <rect width="80%" height="100%" fill="lightgrey" />

    """ // margin on the right for text
  }

  def scatterPlot(points: Vector[(Double, Double, String)],
                  width: Int = 1000,
                  height: Int = 400,
                  r: Double = 3,
                  colour: String = "blue",
                  fontSize: Int = 12) = {

    val xs     = points map (_._1)
    val ys     = points map (_._2)
    val xmax   = xs.max
    val xmin   = xs.min
    val ymax   = ys.max
    val ymin   = ys.min
    val xScale = (width * 0.6 - (4 * r)) / (xmax - xmin)
    val yScale = (height * 0.9 - (4 * r)) / (ymax - ymin)
    val circles =
      points map {
        case (x, y, t) =>
          circ((x - xmin) * xScale + r + r,
               height - r - r - ((y - ymin) * yScale),
               t,
               r,
               colour,
               fontSize)
      }
    s"""
      <div>
      ${header(width, height)}
      ${circles.mkString("\n")}
      </div>
      """
  }
}
