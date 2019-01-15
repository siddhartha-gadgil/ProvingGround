package provingground.interface

import ammonite.ops
import ops._
import Tuts._
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

import scala.util.Try
import scala.xml.Elem

object Site {
  implicit val wd = pwd

  def pack() = {
    pprint.log("packing client")
    %%("mill", "client.pack")
  }

  def mkDocs() = {
    pprint.log("generating scaladocs")
    %%("mill", "jvmRoot.docs")
  }

  def assemble() = {
    pprint.log("assembling mantle")
    %%("mill", "mantle.assembly")
  }

  val mathjax =
    """
      |<!-- mathjax config similar to math.stackexchange -->
      |<script type="text/x-mathjax-config">
      |MathJax.Hub.Config({
      |jax: ["input/TeX", "output/HTML-CSS"],
      |tex2jax: {
      |  inlineMath: [ ['$', '$'] ],
      |  displayMath: [ ['$$', '$$']],
      |  processEscapes: true,
      |  skipTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
      |},
      |messageStyle: "none",
      |"HTML-CSS": { preferredFont: "TeX", availableFonts: ["STIX","TeX"] }
      |});
      |</script>
      |<script type="text/javascript" async
      |      src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML">
      |       </script>
    """.stripMargin

  def head(relDocsPath: String, t: String): String =
    s"""
       |<head>
       |    <meta charset="utf-8">
       |    <meta http-equiv="X-UA-Compatible" content="IE=edge">
       |    <meta name="viewport" content="width=device-width, initial-scale=1">
       |    <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
       |    <title>$t</title>
       |    <link rel="icon" href="${relDocsPath}IIScLogo.jpg">
       |
       |    <!-- Bootstrap -->
       |    <link href="${relDocsPath}css/bootstrap.min.css" rel="stylesheet">
       |   <link href="${relDocsPath}css/katex.min.css" rel="stylesheet">
       |   <link href="${relDocsPath}css/main.css" rel="stylesheet">
       |
       |
       |    <link rel="stylesheet" href="${relDocsPath}css/zenburn.css">
       |    <script src="${relDocsPath}js/highlight.pack.js"></script>
       |    <script>hljs.initHighlightingOnLoad();</script>
       |
       |   <script src="${relDocsPath}js/ace.js"></script>
       |   <script src="${relDocsPath}js/katex.min.js"></script>
       |
       |    $mathjax
       |  </head>
       |
   """.stripMargin

  def nav(relDocsPath: String = ""): Elem =
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
          <ul class="nav navbar-nav" id="left-nav">
            <li><a href={s"${relDocsPath}index.html"}>Docs Home</a></li>
            <li class="dropdown">
              <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                Tutorials (notes)<span class="caret"></span></a>
              <ul class="dropdown-menu">
                {tutList(relDocsPath)}
              </ul>
            </li>
            <li class="dropdown">
              <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                Posts<span class="caret"></span></a>
              <ul class="dropdown-menu">
                {postList(relDocsPath)}
              </ul>
            </li>
          </ul>
          <ul class="nav navbar-nav navbar-right">
            <li> <a href={s"${relDocsPath}scaladoc/provingground/index.html"} target="_blank">ScalaDocs</a></li>
            <li> <a href="https://github.com/siddhartha-gadgil/ProvingGround" target="_blank">
              <img src={s"${relDocsPath}GitHub-Mark-Light-32px.png"} alt="Github"></img> </a> </li>



          </ul>
        </div><!-- /.navbar-collapse -->
      </div><!-- /.container-fluid -->
    </nav>

  def foot(relDocsPath: String): String =
    s"""
       |<div class="container-fluid">
       |  <p>&nbsp;</p>
       |  <p>&nbsp;</p>
       |  <p>&nbsp;</p>
       |  <div class="footer navbar-fixed-bottom bg-primary">
       |    <h4>
       |    &nbsp;Developed by:
       |    &nbsp;<a href="http://math.iisc.ac.in/~gadgil" target="_blank">&nbsp; Siddhartha Gadgil</a>
       |
       |  </h4>
       |
       |  </div>
       |</div>
       |<script type="text/javascript" src="${relDocsPath}js/jquery-2.1.4.min.js"></script>
       |<script type="text/javascript" src="${relDocsPath}js/bootstrap.min.js"></script>
       |<script type="text/javascript" src="${relDocsPath}js/provingground.js"></script>
       |<script>
       |  provingground.main()
       |</script>
   """.stripMargin

  def fromMD(s: String): String = {
    val parser   = Parser.builder().build()
    val document = parser.parse(s)
    val renderer = HtmlRenderer.builder().build()
    renderer.render(document).replace("$<code>", "$").replace("</code>$", "$")
  }

  def threeDash(s: String) = s.trim == "---"

  def withTop(l: Vector[String]) =
    (l.filter(threeDash).size == 2) && threeDash(l.head)

  def body(l: Vector[String]) =
    if (withTop(l)) l.tail.dropWhile((l) => !threeDash(l)).tail else l

  def topmatter(lines: Vector[String]) =
    if (withTop(lines)) Some(lines.tail.takeWhile((l) => !threeDash(l)))
    else None

  def titleOpt(l: Vector[String]): Option[String] =
    for {
      tm <- topmatter(l)
      ln <- tm.find(_.startsWith("title: "))
    } yield ln.drop(6).trim

  def filename(s: String) = s.toLowerCase.replaceAll("\\s", "-")

  case class Tut(name: String, rawContent: String, optTitle: Option[String]) {
    val title = optTitle.getOrElse(name)

    val target = pwd / "docs" / "tuts" / s"$name.html"

    def url(relDocsPath: String) = s"${relDocsPath}tuts/$name.html"

    def content: String = fromMD(mkTut(rawContent))

    def output: String =
      page(content, "../", title)

    def save = write.over(target, output)

    def json: ujson.Obj = {
      import ujson._
      ujson.Obj("name" -> name, "title" -> title)
    }
  }

  def getTut(p: Path): Tut = {
    val l = ops.read.lines(p).toVector
    val name =
      titleOpt(l).map(filename).getOrElse(
        p.last.dropRight(p.ext.length + 1)
        )
    val rawContent = body(l).mkString("\n")
    Tut(name, rawContent, titleOpt(l))
  }

  def allTuts: Seq[Tut] = ls(tutdir).map(getTut)

  def tutList(relDocsPath: String): Seq[Elem] =
    Try {
      allTuts.map(
        (tut) => <li><a href={s"${tut.url(relDocsPath)}"}>{tut.title}</a></li>
      )
    }.orElse(
        Try {
          val jsArr = ujson.read(read(resource / "tut-list.json"))
          jsArr.arr.map { (js) =>
            // pprint.log(js)
            val name        = js.obj("name").str
            val title       = js.obj("title").str
            val url: String = s"${relDocsPath}tuts/$name.html"
            <li>
            <a href={url}>
              {title}
            </a>
          </li>
          }

        }
      )
      .getOrElse(Vector())

  def dateOpt(l: Vector[String]): Option[(Int, Int, Int)] =
    for {
      tm <- topmatter(l)
      m <- """date: (\d\d\d\d)-(\d\d)-(\d\d)""".r.findFirstMatchIn(
        tm.mkString("\n"))
    } yield (m.group(1).toInt, m.group(2).toInt, m.group(3).toInt)

  case class Post(name: String,
                  content: String,
                  optDate: Option[(Int, Int, Int)],
                  optTitle: Option[String]) {
    lazy val title = optTitle.getOrElse(name)

    lazy val dateString =
      optDate.map { case (y, m, d) => s"$y-$m-$d-" }.getOrElse("")

    val target = pwd / "docs" / "posts" / s"$name.html"

    def url(relDocsPath: String) = s"${relDocsPath}posts/$name.html"

    val date: (Int, Int, Int) = optDate.getOrElse((0, 0, 0))

    def output: String =
      page(fromMD(content), "../", title)

    def save = write.over(target, output)

    def json: ujson.Obj = {
      import ujson._
      ujson.Obj("name" -> name, "title" -> title, "date" -> dateString)
    }
  }

  def getPost(p: Path): Post = {
    val l = ops.read.lines(p).toVector
    val name =
      titleOpt(l).map(filename).getOrElse(
        p.last.dropRight(p.ext.length + 1)
        )
    val content = body(l).mkString("\n")
    Post(name, content, dateOpt(l), titleOpt(l))
  }

  def postsDir = pwd / "jekyll" / "_posts"

  def allPosts: Seq[Post] =
    ls(postsDir).map(getPost).sortBy(_.date).reverse

  def postList(relDocsPath: String): Seq[Elem] =
    Try {
      allPosts.map(
        (post) =>
          <li><a href={s"${post.url(relDocsPath)}"}>{post.dateString + post.title}</a></li>
      )
    }.orElse(
        Try {
          val jsArr = ujson.read(read(resource / "posts-list.json"))
          jsArr.arr.map {
            (js) =>
              // pprint.log(js)
              val name        = js.obj("name").str
              val dateString  = js.obj("date").str
              val title       = js.obj("title").str
              val url: String = s"${relDocsPath}posts/$name.html"
              <li>
            <a href={url}>
              {dateString + title}
            </a>
          </li>
          }
        }
      )
      .getOrElse(Vector())

  def page(s: String,
           relDocsPath: String,
           t: String = "ProvingGround",
           haltButton: Boolean = false): String =
    s"""
       |<!DOCTYPE html>
       |<html lang="en">
       |${head(relDocsPath, t)}
       |<body>
       |${nav(relDocsPath)}
       |<div class="container">
       |${if (haltButton)
         """<a href="halt" target="_blank" class="btn btn-danger pull-right">Halt Server</a>"""
       else ""}
       |<h1 class="text-center">$t</h1>
       |
       |<div class="text-justify">
       |$s
       |
       |</div>
       |</div>
       |${foot(relDocsPath)}
       |</body>
       |</html>
   """.stripMargin

  def home =
    page(fromMD(
           body(ops.read.lines(pwd / "docs" / "index.md").toVector)
             .mkString("", "\n", "")),
         "",
         "ProvingGround: Automated Theorem proving by learning")

  def mkHome(): Unit =
    write.over(pwd / "docs" / "index.html", home)

  def mkLists() = {
    val tutsJs = ujson.Arr(allTuts.map(_.json): _*)
    write.over(pwd / "docs" / "tut-list.json", tutsJs.toString)
    val postsJs = ujson.Arr(allPosts.map(_.json): _*)
    write.over(pwd / "docs" / "posts-list.json", postsJs.toString())
  }

  def mkTuts() = {
    allTuts.foreach { (tut) =>
      pprint.log(s"compiling tutorial ${tut.name}")
      write.over(tut.target, tut.output)
    }
  }

  def mkPosts() = {
    allPosts.foreach { (post) =>
      pprint.log(s"saving post ${post.name} written on ${post.dateString}")
      write.over(post.target, post.output)
    }
  }

  def mkSite(): Unit = {
    println("writing site")

    mkLists()

    pack()

    mkDocs()

    assemble()

    mkHome()

    mkTuts()

    mkPosts()
  }

}
