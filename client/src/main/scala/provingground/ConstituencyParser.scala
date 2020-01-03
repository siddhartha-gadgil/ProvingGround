package provingground

import org.scalajs.dom
import scalajs.js.annotation._
import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom

import js.Dynamic.{global => g}

// import com.scalawarrior.scalajs.ace._

import dom.ext._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// import ujson.Js

import scala.util.{Try, Success, Failure}

import HoTT.{id => _, _}, translation._
@JSExportTopLevel("parser")
object ConstituencyParser {
  @JSExport
  def load(): Unit = {
    val navDiv = dom.document.querySelector("#left-nav")
    navDiv.appendChild(
      li(
        a(href := "./hott/", target := "_blank")("HoTT Server")
      ).render
    )
    val runButton =
      input(`type` := "button",
            value := "Parse (ctrl-B)",
            `class` := "btn btn-success").render
    val treeDiv    = div(`class` := "panel-body view")().render
    val exprDiv    = div(`class` := "panel-body language-scala view")().render
    val depTreeDiv = div(`class` := "panel-body view")().render
    val logDiv     = div()().render
    val parseInput =
      input(`type` := "text", `class` := "form-control").render

    def parse(txt: String) = {
      runButton.value = "Parsing..."

      Ajax.post("parse", txt).foreach { (xhr) =>
        {
          logDiv.appendChild(p("button clicked").render)
          val answer = xhr.responseText
          runButton.value = "Parse (ctrl-B)"
          logDiv.appendChild(pre(answer).render)
          val js   = ujson.read(answer)
          val tree = js.obj("tree").str.toString
          treeDiv.innerHTML = ""
          treeDiv.appendChild(pre(tree).render)
          val expr =
            js.obj("expr")
              .str
              .toString
          val parsed = js.obj("parsed").bool
          exprDiv.innerHTML = ""
          exprDiv.appendChild(
            div(
              p(s"Parsed: $parsed"),
              pre(
              code(`class` := "language-scala")(expr)
            )
              ).render)
          val depTree = js.obj("deptree")
          depTreeDiv.innerHTML = ujson.read(depTree).str
          g.hljs.highlightBlock(exprDiv)
          g.hljs.initHighlighting.called = false
          g.hljs.initHighlighting()
        }
      }
    } //parse

    runButton.onclick = (e: dom.Event) => parse(parseInput.value)

    dom.window.onkeydown = (e) => {
      if (e.ctrlKey && e.keyCode == 66) parse(parseInput.value)
    }

    // haltButton.onclick = (e: dom.Event) => Ajax.get("halt")

    val assertions =
      Vector(
        "Every natural number is greater than $0$", //parsed
        "Every natural number $n$, where $n$ is greater than $1$, is divisible by a prime number", //parsed
        "Every natural number $n$ which is greater than $1$ is divisible by a prime number", //parsed
        "if a prime number $p$ divides the product of $m$ and $n$,  $p$ divides one of $m$ and $n$",
        "if a prime number $p$ divides the product of $m$ and $n$,  $p$ divides $m$ or $n$", //parsed
        "if a prime number $p$ divides $mn$, $p$ divides $m$ or $n$", //parsed
        "Every natural number which is greater than $1$ is divisible by a prime number",
        "Every natural number $n$, which is greater than $1$, is divisible by a prime number", //parsed
        // "Six is not the sum of two distinct primes",
        "$6$ is not the sum of two distinct prime numbers", // 'primes' causes trouble
        "Every natural number is greater than $0$",
        "Every number $n$ is divisible by a prime number $p$",
        // "The image of $Z$ in $K$ is an integral domain, hence isomorphic to $Z$ or $Z/p$, where $p$ is a prime",
        "If $G/H$ is cyclic, then $G$ is abelian", // parsed
        "If $G/H$ is cyclic, $G$ is abelian", //parsed
        "$\\ker \\phi$ is the set of all $a\\in A$ that map to an element in $B$",
        "$6$ is not the square of a prime", //parsed
        "$6$ is not the square of all prime numbers", //parsed,
        "$6$ is not the sum of distinct prime numbers",
        "$6$ is not the square of all primes",
        // "An abelian group is finitely generated if and only if the corresponding $Z$-module is finitely generated",
        "$G$ is solvable if there exists $n in \\mathbb{N}$ such that $G^{(n)}=1$", //parsed
        "there are $n,m\\in \\mathbb{Z}$ such that $xH = (gH)^n = g^nH$",
        // "Two quadratic forms over $k$ are equivalent if and only if they have the same rank",
        "Two quadratic forms over $k$ are equivalent if and only if they have the same rank", //experiment iff -> and
        "Two quadratic forms over $k$ are equivalent if and only if they have the same rank, same discriminant and same invariant $\\epsilon$",
        "The discriminant of $g$ is equal to $d/a$", //parsed
        // "The number of elements of $k/k^2$ which are represented by $f$ is equal to $1$",
        // "The number of elements of $k/k^2$ which are represented by $f$ is equal to $1$ if $n=1$, to $2^r -1$ if $n=3$, and to $2^r$ if $n=4$",
        "if $p$ is a prime number, the form deduced from $f$ by reduction modulo $p$ has a non-trivial root",
        "if $p$ is a prime number, the form deduced from $f$ by reduction modulo $p$ has a non-trivial zero, and this zero can be lifted to a p-adic zero",
        "the quadratic form $f$ represents zero in all the $Q_p$, and also in $R$",
        "if two diagrams $D_1$ and $D_2$ are related by a chain of moves, the complexes of  groups $C(D_1)$ and $C(D_2)$ are equivalent",
        "if two diagrams $D_1$ and $D_2$ are related by a chain of Reidemeister moves, the complexes of  groups $C(D_1)$ and $C(D_2)$ are equivalent",
        "if two diagrams $D_1$ and $D_2$ are related by a chain of Reidemeister moves, the complexes of graded abelian groups $C(D_1)$ and $C(D_2)$ are equivalent and homology groups $H(D_1)$ and $H(D_2)$ are isomorphic",
        "$AB \\subset G$ if and only if $AB = BA$", //parsed
        "$AB \\subset G$ and $AB = BA$", //experiment iff -> and; parsed
        "$[A,B] = \\{e\\}$ if and only if $ab = ba, \\forall a \\in A, b \\in B$ if and only if $A \\subset C_G(B)$ if and only if $B \\subset C_G(A)$"
      )

    def egSpan(t: String) = {
      val cp = button(`class` := "btn btn-info")("copy").render
      cp.onclick = (_) => parseInput.value = t
      val pb = button(`class` := "btn btn-success")("parse").render
      pb.onclick = (_) => {
        parseInput.value = t
        parse(t)
      }
      span(pb, span(" "), cp, span(" "), t)
    }

    val exampleList = ul(`class` := "list-group")(
      assertions.map((t) => li(`class` := "list-group-item")(egSpan(t))): _*
    )

    val jsDiv =
      div(
        p("""This is an interface for experimenting with the constituency parser based translation
            |from sentences to mathematical expressions. Example sentences to try are at the end of this page.
            |Enter a sentence to parse. You will see:""".stripMargin),
        ul(
          li("the constituency parsed tree produced by the stanford parser."),
          li("the mathematical expression to which it translates recursively.")
        ),
        p("If some node/leaf fails to translate, it is translated to a ",
          code("FormalNode"),
          " or ",
          code("FormalLeaf")),
        p(strong("Warning: "),
          "occasionally the server may crash, so you will need to restart it."),
        form(div(`class` := "form-group")(label("Sentence:"), parseInput),
             runButton),
        p(),
        div(`class` := "panel panel-primary")(
          div(`class` := "panel-heading")(
            h4("Constituency parsed tree"),
            p("the output of the stanford parser")),
          treeDiv),
        div(`class` := "panel panel-success")(
          div(`class` := "panel-heading")(
            h4("Mathematical Expression"),
            p("an expression in a structure modelled on Naproche CNL")),
          exprDiv),
        div(`class` := "panel panel-info")(
          div(`class` := "panel-heading")(h4("Dependency parsed tree"),
                                          p("not used at present")),
          depTreeDiv),
        p(),
        h3("Example Sentences"),
        p("Warning: Enter sentences in TeX notation, i.e, with dollars surrounding formulas. The copy button does this."),
        p(),
        exampleList
      )

    val pdiv = dom.document.querySelector("#constituency-parser")
    // (pdiv) =>
    val parseDiv = pdiv.asInstanceOf[org.scalajs.dom.html.Div]

    parseDiv.appendChild(jsDiv.render)

    // val hDiv = dom.document.querySelector("#halt")
    // hDiv.appendChild(haltButton)

    // } // option ma
  } // load
}
