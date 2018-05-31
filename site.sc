#!/usr/bin/env amm

import scala.xml.Elem


import scala.xml.NodeBuffer


import ammonite.ops.{%%, _}

implicit val wd = pwd

println("packing client")

%%("mill", "client.pack")

import $file.tuts
import tuts._

println("Generating scaladocs")

%%("mill", "jvmRoot.docs")

val mathjax = """    <!-- mathjax config similar to math.stackexchange -->
<script type="text/x-mathjax-config">
MathJax.Hub.Config({
jax: ["input/TeX", "output/HTML-CSS"],
tex2jax: {
  inlineMath: [ ['$', '$'] ],
  displayMath: [ ['$$', '$$']],
  processEscapes: true,
  skipTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
},
messageStyle: "none",
"HTML-CSS": { preferredFont: "TeX", availableFonts: ["STIX","TeX"] }
});
</script>
<script type="text/javascript" async
      src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML">
       </script>
"""

def head(rel: String = "") =
s"""
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
    <title>ProvingGround</title>
    <link rel="icon" href="${rel}IIScLogo.jpg">

    <!-- Bootstrap -->
    <link href="${rel}css/bootstrap.min.css" rel="stylesheet">
   |<link href="${rel}css/katex.min.css" rel="stylesheet">
   |<link href="${rel}css/main.css" rel="stylesheet">


    <link rel="stylesheet" href="${rel}css/zenburn.css">
    <script src="${rel}js/highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>

   |<script src="${rel}js/ace.js"></script>
   |<script src="${rel}js/katex.min.js"></script>

    $mathjax
  </head>

"""
  def nav(rel: String = ""): Elem =
//    s"""
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
            <li><a href={s"${rel}index.html#"}>Home</a></li>
            <li class="dropdown">
              <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                Tutorials (notes)<span class="caret"></span></a>
              <ul class="dropdown-menu">
                <li class="dropdown-header"><strong>Tutorials (notes)</strong></li>
                {tutList(rel)}
            </ul>
          </li>
      </ul>
          <ul class="nav navbar-nav navbar-right">
            <li> <a href={s"${rel}scaladoc/provingground/index.html"} target="_blank">ScalaDocs</a></li>
            <li> <a href="https://github.com/siddhartha-gadgil/ProvingGround" target="_blank">
            <img src={s"${rel}GitHub-Mark-Light-32px.png"} alt="Github"></img> </a> </li>



          </ul>
        </div><!-- /.navbar-collapse -->
      </div><!-- /.container-fluid -->
    </nav>

// """

def foot(rel: String): String = s"""
<div class="container-fluid">
  <p>&nbsp;</p>
  <p>&nbsp;</p>
  <p>&nbsp;</p>
  <div class="footer navbar-fixed-bottom bg-primary">
    <h4>
    &nbsp;Developed by:
    &nbsp;<a href="http://math.iisc.ac.in/~gadgil" target="_blank">&nbsp; Siddhartha Gadgil</a>

  </h4>

  </div>


</div>
<script type="text/javascript" src="${rel}js/jquery-2.1.4.min.js"></script>
<script type="text/javascript" src="${rel}js/bootstrap.min.js"></script>
<script type="text/javascript" src="${rel}js/provingground.js"></script>
<script>
  provingground.main()
</script>
"""

import $ivy.`com.atlassian.commonmark:commonmark:0.11.0`
import org.commonmark.node._
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

def fromMD(s: String) : String = {
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
  println(s"Compiling tutorial ${p.name}")
  val l = mkTut(read(p)).split("\n").toVector
  val name = titleOpt(l).map(filename).getOrElse(p.name.dropRight(p.ext.length + 1))
  val content = fromMD(body(l).mkString("\n"))
  Tut(name, content, titleOpt(l))
  }

lazy val allTuts = ls(tutdir).map(getTut)

def tutList(rel: String)  =
    allTuts.map(
      (tut) =>
        <li><a href={s"${tut.url(rel)}"}>{tut.title}</a></li>
      )


def doc(s: String, rel: String, t: String = "") =
s"""
<!DOCTYPE html>
<html lang="en">
${head(rel)}
<body>
${nav(rel)}
<div class="container">
<h1 class="text-center">$t</h1>

<div class="text-justify">
$s

</div>
</div>
${foot(rel)}
</body>
</html>
"""

val home = doc(
  fromMD(body(read.lines(pwd / "docs" /"index.md").toVector).mkString("", "\n", "")), "", "ProvingGround: Automated Theorem proving by learning")

println("writing site")

write.over(pwd / "docs" / "index.html", home)

allTuts.foreach((tut) => write.over(tut.target, tut.output))
