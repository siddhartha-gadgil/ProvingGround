import $file.tuts, tuts._

import ammonite.ops._

def head(rel: String = "") =
s"""<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
    <title>ProvingGround</title>
    <link rel="icon" href="{{ site.baseurl }}/IIScLogo.jpg">

    <!-- Bootstrap -->
    <link href="$rel/css/bootstrap.min.css" rel="stylesheet">


    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
    <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
    <!--[if lt IE 9]>
      <script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
      <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->

  </head>
  <body>

    <nav class="navbar navbar-default">
      <div class="container-fluid">
        <!-- Brand and toggle get grouped for better mobile display -->
        <div class="navbar-header">
          <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
            <span class="sr-only">Toggle navigation</span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </button>
          <span class="navbar-brand">ProvingGround</span>
        </div>

        <!-- Collect the nav links, forms, and other content for toggling -->
        <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
          <ul class="nav navbar-nav">
            <li><a href="${rel}index.html#">Home</a></li>
            <li class="dropdown">
              <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                Tutorials (notes)<span class="caret"></span></a>
              <ul class="dropdown-menu">
                <li class="dropdown-header"><strong>Tutorials (notes)</strong></li>
                ${tutList(rel)}
            </ul>
          </li>
      </ul>
          <ul class="nav navbar-nav navbar-right">
            <li> <a href="https://github.com/siddhartha-gadgil/ProvingGround" target="_blank">Github repository</a> </li>



          </ul>
        </div><!-- /.navbar-collapse -->
      </div><!-- /.container-fluid -->
    </nav>

    <div class="container">
"""

val foot = s"""</div>
<div class="container-fluid">
  <br><br><br>
  <div class="footer navbar-fixed-bottom bg-primary">
    <h4>
    &nbsp;<a href="http://math.iisc.ac.in" target="_blank">&nbsp; Department of Mathematics,</a>

    &nbsp;<a href="http://iisc.ac.in" target="_blank">Indian Institute of Science.</a>
  </h4>

  </div>


</div>
"""

import $ivy.`com.atlassian.commonmark:commonmark:0.11.0`
import org.commonmark.node._
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

def fromMD(s: String) = {
    val parser = Parser.builder().build()
    val document = parser.parse(s)
    val renderer = HtmlRenderer.builder().build()
    renderer.render(document).replace("$<code>", "$").replace("</code>$", "$")
  }


def threeDash(s: String) = s.trim == "---"

def withTop(l: Vector[String]) = (l.filter(threeDash).size == 2) && threeDash(l.head)

def body(l: Vector[String]) = if (withTop(l)) l.tail.dropWhile((l) => !threeDash(l)).tail else l

def topmatter(lines: Vector[String]) = if (withTop(lines))  Some(lines.tail.takeWhile((l) => !threeDash(l))) else None

def titleOpt(l: Vector[String]) =
  for {
    tm <- topmatter(l)
    ln <- tm.find(_.startsWith("title: "))
    } yield ln.drop(6).trim

def filename(s: String) = s.toLowerCase.replaceAll("\\s", "-")

case class Tut(name: String, content: String, optTitle: Option[String]){
  val title = optTitle.getOrElse(name)

  val target = pwd / "docs" / "tuts"/ s"$name.html"

  def url(rel: String) = s"${rel}tuts/$name.html"

  lazy val output =
    doc(
      content,
      "../",
      title)

  def save = write.over(target, output)
}

def getTut(p: Path) =
  {
  val l = mkTut(p).split("\n").toVector
  val name = titleOpt(l).map(filename).getOrElse(p.name.dropRight(p.ext.length + 1))
  val content = fromMD(body(l).mkString("\n"))
  Tut(name, content, titleOpt(l))
  }

lazy val allTuts = ls(tutdir).map(getTut)

def tutList(rel: String)  =
    allTuts.map(
      (tut) =>
        s"""<li><a href="${tut.url(rel)}">${tut.title}</a></li>"""
      ).mkString("", "\n", "")


def doc(s: String, rel: String, t: String = "") =
s"""
${head(rel)}
<h1 class="text-center">$t</h1>\n
<div class="col-md-8">
$s

</div>
$foot
"""

val home = doc(
  fromMD(read(pwd / "docs" /"index.md")), "", "ProvingGround: Automated Theorem proving By learning")

println("writing site")

write.over(pwd / "docs" / "index.html", home)

allTuts.foreach((tut) => write.over(tut.target, tut.output))
