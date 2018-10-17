package provingground

import provingground.{FiniteDistribution => FD}
import org.scalajs.dom

import scalajs.js.annotation._
import scalatags.JsDom.all._
import org.scalajs.dom.raw._

import scala.scalajs.js
import org.scalajs.dom
import dom.ext._
import monix.execution.Scheduler.Implicits.global
import monix.eval._
import HoTT.{id => _, _}
import translation._
import learning._
import FineProverTasks._
import com.scalawarrior.scalajs.ace.ace
import monix.execution.CancelableFuture
import org.scalajs.dom.html.{Div, Input, Span, Table, TableRow}
import provingground.scalahott.NatRing
import scalatags.JsDom
import ujson.Js
import upickle.default._

import scala.util.Try

object FDInput {
  def uniform[A](v: Vector[A]) = {
    val m =
      if (v.isEmpty) Map.empty[A, Double]
      else v.map((x) => x -> 1.0 / v.size).toMap
    FDInput(v, m)
  }
}

case class FDInput[A](elems: Vector[A], previous: Map[A, Double]) {
  val inputPairs: Vector[(A, Input)] =
    for {
      x <- elems
      p   = previous.getOrElse(x, 0.0)
      inp = input(`type` := "text", value := p, size := 3).render
    } yield (x, inp)
  val inputRows: Vector[JsDom.TypedTag[TableRow]] =
    for {
      (x, p) <- inputPairs
    } yield tr(td(x.toString), td(p))

  def outputPairs: Vector[(A, Double)] =
    for {
      (x, inp) <- inputPairs
      p = Try(inp.value.toDouble).getOrElse(0.0)
    } yield (x, p)

  def fd: FD[A] =
    FD(for {
      (x, p) <- outputPairs
    } yield Weighted(x, p))

  def total: Double = fd.total

  val totalSpan: Span = span(total).render

  val totalRow = tr(td(strong("Total:")), td(totalSpan))

  def update(): Unit = {
    totalSpan.textContent = total.toString
  }

  inputPairs.map(_._2).foreach { (el) =>
    el.oninput = (_) => update()
  }

  val view: JsDom.TypedTag[Table] =
    table(`class` := "table table-striped")(
      tbody(
        inputRows :+ totalRow: _*
      )
    )
}

case class DoublesInput(elems: Vector[String], default: Map[String, Double]) {
  val inputPairs: Vector[(String, Input)] =
    for {
      x <- elems
      p   = default.getOrElse(x, 0.0)
      inp = input(`type` := "text", value := p, size := 3).render
    } yield (x, inp)

  val inputRows: Vector[JsDom.TypedTag[TableRow]] =
    for {
      (x, p) <- inputPairs
    } yield tr(td(x.toString), td(p))

  def outputPairs: Vector[(String, Double)] =
    for {
      (x, inp) <- inputPairs
      p = Try(inp.value.toDouble).getOrElse(0.0)
    } yield (x, p)

  def outputMap: Map[String, Double] = outputPairs.toMap

  val view: JsDom.TypedTag[Table] =
    table(`class` := "table table-striped")(
      tbody(
        inputRows: _*
      )
    )
}

object DoublesInput {
  def apply(kvs: (String, Double)*): DoublesInput =
    DoublesInput(kvs.toVector.map(_._1), kvs.toMap)
}

@JSExportTopLevel("interactiveProver")
object InteractiveProver {
  import katexSafe.teXSpan

  def entropyTable[U <: Term with Subs[U]](
      fd: FD[U],
      typed: Boolean = true): JsDom.TypedTag[Table] = {
    val header =
      if (typed)
        thead(
          tr(th("term"), th("type"), th("entropy"))
        )
      else
        thead(
          tr(th("term"), th("entropy"))
        )

    val rows =
      for {
        Weighted(x, p) <- fd.entropyVec
      } yield
        if (typed)
          tr(td(teXSpan(x)), td(teXSpan(x.typ)), td(f"$p%1.3f"))
        else tr(td(teXSpan(x)), td(f"$p%1.3f"))
    table(`class` := "table")(
      header,
      tbody(rows: _*)
    )
  }

  def thmTable(ts: TermState): JsDom.TypedTag[Table] = {
    val rows =
      for {
        (t, p, q, h) <- ts.thmWeights
      } yield
        tr(
          td(teXSpan(t)),
          td(f"$p%1.4f"),
          td(f"$q%1.4f"),
          td(f"$h%1.3f")
        )
    val header =
      tr(th("theorem"),
         th("statement prob"),
         th("proof prob"),
         th("entropy-term"))
    table(`class` := "table")(
      header,
      tbody(rows: _*)
    )
  }

  def termStateView(ts: TermState): JsDom.TypedTag[Div] =
    div(`class` := "panel panel-info")(
      div(`class` := "panel-heading")(h3("Evolved Term State")),
      div(`class` := "panel-body")(
        div(`class` := "view")(
          h4("Entropies of Terms"),
          entropyTable(ts.terms)
        ),
        div(`class` := "view")(
          h4("Entropies of Types"),
          entropyTable(ts.typs, typed = false)
        ),
        div(`class` := "view")(
          h4("Theorems"),
          thmTable(ts)
        )
      )
    )

  @JSExport
  def load(): Unit = {

    val proverDivOpt: Option[Element] = Option(
      dom.document.querySelector("#interactive-prover-div"))
    proverDivOpt.foreach { proverDiv =>
      val logList = ul(`class` := "view").render

      def log(s: String) = {
        logList.appendChild(li(s).render)
      }

      val worksheet: Div = div(p()).render

      def show(el: Element): Unit =
        worksheet.appendChild(el)

      var context: Context = //Context.Empty
        NatRing.context

      var termsInput: FDInput[Term] = FDInput.uniform(context.terms)

      val termsInputDiv = div(`class` := "col-md-4")(h3("Terms Distribution"),
                                                     termsInput.view).render

      var typsInput = FDInput.uniform(context.terms.flatMap(typOpt))

      val typsInputDiv = div(`class` := "col-md-4")(h3("Types Distribution"),
                                                    typsInput.view).render

      val paramsInput =
        DoublesInput(
          "function-application"    -> 0.1,
          "unified-application"     -> 0.1,
          "application-by-argument" -> 0.1,
          "lambda"                  -> 0.1,
          "pi"                      -> 0.1,
          "terms-by-type"           -> 0.05,
          "type-from-family"        -> 0.05,
          "variable-weight"         -> 0.3,
          "goal-weight"             -> 0.5,
          "epsilon"                 -> 0.001
        )

      def tg: TermGenParams = {
        val m = paramsInput.outputMap
        TermGenParams(
          m("function-application"),
          m("unified-application"),
          m("application-by-argument"),
          m("lambda"),
          m("pi"),
          m("terms-by-type"),
          m("type-from-family"),
          m("variable-weight"),
          m("goal-weight")
        )
      }

      def epsilon: Double = paramsInput.outputMap("epsilon")

      def updateTermsInput(): Unit = {
        termsInput = FDInput.uniform(context.terms)
        termsInputDiv.innerHTML = ""
        termsInputDiv.appendChild(
          div(h3("Terms Distribution"), termsInput.view).render
        )

        typsInput = FDInput.uniform(context.terms.flatMap(typOpt))
        typsInputDiv.innerHTML = ""
        typsInputDiv.appendChild(
          div(h3("Types Distribution"), typsInput.view).render
        )
      }

      val ed = div(id := "editor", `class` := "panel-body editor")

      val viewDiv = div(`class` := "view panel-body")().render

      val runButton =
        input(`type` := "button",
              value := "Update Context (ctrl-B)",
              `class` := "btn btn-success").render

      val scratch = span(contenteditable := true)("Some stuff").render

      val echo = span(scratch.textContent).render

      scratch.oninput = (_) => {
        echo.textContent = scratch.textContent
      }

      val stepButton =
        input(`type` := "button",
              value := "Term Generation Step",
              `class` := "btn btn-primary").render

      val mainDiv =
        div(
          div(`class` := "row")(
            div(`class` := "panel panel-primary col-md-7")(
              div(`class` := "panel-heading")(
                h4("Context Editor"),
                p("The context can be used for definitions including inductive types.")),
              ed,
              div(`class` := "panel-footer")(runButton)
            ),
            div(`class` := "panel panel-success col-md-5")(
              div(`class` := "panel-heading")(h4("Parsed Context")),
              viewDiv)
          ),
          h2("Term Generation"),
          termsInputDiv,
          typsInputDiv,
          div(`class` := "col-md-4")(h3("Term Generator Parameters"),
                                     paramsInput.view),
          stepButton,
          worksheet,
          h3("Logs"),
          logList
        ).render

      val parser = HoTTParser(
//        Context.Empty
        NatRing.context
      )

      proverDiv.appendChild(
        div(
          mainDiv
        ).render
      )

      val editor = ace.edit("editor")
      editor.setTheme("ace/theme/chrome")
      editor.getSession().setMode("ace/mode/scala")

      def compile(): Unit = {
        val text = editor.getValue

        val view: JsDom.TypedTag[Div] =
          Try(
            parser.context
              .parse(text))
            .fold(
              fa =>
                div(
                  h5(`class` := "text-danger")("Exception while parsing"),
                  div(fa.getMessage)
              ),
              fb =>
                fb.fold(
                  (_, _, s) =>
                    div(
                      h5(`class` := "text-danger")("Parsing failure"),
                      div(s.traced.trace)
                  ),
                  (ctx, _) => {
                    context = ctx
                    updateTermsInput()
                    div(
                      h5(`class` := "text-success")("Context Parsed"),
                      ctx.valueOpt
                        .map {
                          (t) =>
                            val termSpan = span().render
                            val typSpan  = span().render
                            termSpan.innerHTML =
                              katexSafe.renderToString(TeXTranslate(t))
                            typSpan.innerHTML =
                              katexSafe.renderToString(TeXTranslate(t.typ))
                            div(ul(`class` := "list-inline")(li("Term: ",
                                                                termSpan),
                                                             li("Type: ",
                                                                typSpan)),
                                p("Context: ", ctx.toString),
                                p("All Terms"),
                                ctx.terms.mkString(", "))
                        }
                        .getOrElse(div("Empty Context"))
                    )
                  }
              )
            )

        viewDiv.innerHTML = ""
        viewDiv.appendChild(view.render)
      }

      runButton.onclick = (event: dom.Event) => compile()

      mainDiv.onkeydown = (e) => {
        if (e.ctrlKey && e.keyCode == 66) compile()
      }

      val chat: WebSocket =
        new WebSocket(s"ws://${dom.document.location.host}/prover-websock")

      chat.onopen = (_) => log("Web Socket open")

      def step(): Unit = {

        import interface._, TermJson._
        val initialState =
          Js.Obj(
            "terms"                -> fdJson(termsInput.fd),
            "types"                -> fdJson(typsInput.fd map ((t) => t: Term)),
            "goals"                -> fdJson(FD.empty),
            "variables"            -> Js.Arr(),
            "inductive-structures" -> InducJson.toJson(context.inducStruct),
            "context"              -> ContextJson.toJson(context)
          )

        val js =
          Js.Obj(
            "job" -> "step",
            "data" -> Js.Obj(
              "epsilon"              -> Js.Num(epsilon),
              "generator-parameters" -> write(tg),
              "initial-state"        -> initialState
            )
          )

        chat.send(ujson.write(js))
      }

      stepButton.onclick = (_) => step()

      chat.onmessage = { (event: MessageEvent) =>
        val msg = event.data.toString

        val stateTry = Try(
          TermState.fromJson(ujson.read(ujson.read(msg).obj("result").str))
        )
        stateTry.foreach((ts) => show(termStateView(ts).render))
//        val termsVecTry = stateTry.map(_.terms.entropyVec)
//        termsVecTry.foreach { (v) =>
//          v.foreach {
//            case Weighted(elem, weight) => log(s"$elem -> $weight")
//          }
//        }
      }

    }

  }
}

@JSExportTopLevel("prover")
object ProverClient {
  @JSExport
  def load(): Unit = {
    val runButton =
      input(`type` := "button",
            value := "Ask Server for proof",
            `class` := "btn btn-primary").render

    val proverDiv = dom.document.querySelector("#prover-div")
    proverDiv.appendChild(
      div(
        p("""
          |This is a demonstration of autonomous proving.
          |The key feature of our approach is that we judge the value of intermediate lemmas in a
          |manner that emphasises parsimony of the generative model. This means that for a
          |term to be considered an interesting lemma:
        """.stripMargin),
        ul(
          li(
            "Its statement must be simple compared to the proof, and moreover"),
          li("the difference in complexity should be enough given a significant cost for noting an additional lemma")
        ),
        p("The example result we have is showing that if left and right identities exist, they are equal"),
        runButton
      ).render
    )

    // val sse = new dom.EventSource("./proof-source")
    //
    // sse.onmessage = (event: dom.MessageEvent) => showProof(event.data.toString)

    def showProof(data: String): Unit =
      if (data.nonEmpty) {
        runButton.value = "Ask Server for proof"
        val answer = data
        val js     = ujson.read(answer)
        val proved = js.obj("proved").bool
        if (proved) {
          val termDiv = div(style := "overflow-x: auto;")().render
          val typDiv  = div(style := "overflow-x: auto;")().render
          termDiv.innerHTML = katexSafe.renderToString(js.obj("term").str)
          typDiv.innerHTML = katexSafe.renderToString(js.obj("type").str)

          proverDiv.appendChild(
            ul(`class` := "list-group")(
              li(`class` := "list-group-item list-group-item-primary")(
                "From the server via WebSocket:"),
              li(`class` := "list-group-item list-group-item-info")("Theorem"),
              li(`class` := "list-group-item")(typDiv),
              li(`class` := "list-group-item list-group-item-success")("Proof"),
              li(`class` := "list-group-item")(termDiv),
            ).render)

          val lemmaSeq = js.obj("lemmas").arr
          def lemmaLI(lm: Js.Value) = {
            val termDivL = div(style := "overflow-x: auto;")().render
            val typDivL  = div(style := "overflow-x: auto;")().render
            termDivL.innerHTML = katexSafe.renderToString(lm.obj("term").str)
            typDivL.innerHTML = katexSafe.renderToString(lm.obj("type").str)
            li(`class` := "list-group-item")(
              ul(`class` := "list-group")(
                li(`class` := "list-group-item list-group-item-info")("Lemma"),
                li(`class` := "list-group-item")(typDivL),
                li(`class` := "list-group-item list-group-item-success")(
                  "Proof"),
                li(`class` := "list-group-item")(termDivL),
              )
            )
          }
          val lemmaLISeq = lemmaSeq.map(lemmaLI)
          proverDiv.appendChild(
            div(
              h3("Lemmas:"),
              ul(`class` := "list-group")(
                // li(`class` := "list-group-item list-group-item-warning")("Lemmas:"),
                lemmaLISeq: _*
              )
            ).render
          )

        } else
          proverDiv.appendChild(h3("could not find proof of theorem").render)

      }

    def queryPost(): Unit = {
      runButton.value = "Asking Server"
      Ajax
        .post("./monoid-proof")
        .foreach { (xhr) =>
          runButton.value = xhr.responseText
        }
    }

    runButton.onclick = (e: dom.Event) => queryPost() // change this to query a websocket

    val chat = new WebSocket(
      s"ws://${dom.document.location.host}/monoid-websock")

    chat.onopen = { (event: Event) =>
      runButton.value = "Ask Server (over websocket)"
      runButton.onclick = (e: dom.Event) => {
        runButton.value = "Asking Server (over WebSocket)"
        chat.send("monoid-proof")
      }
    }

    chat.onmessage = { (event: MessageEvent) =>
      val msg = event.data.toString
      if (msg == "monoid-proof") runButton.value = "Server Working"
      else showProof(msg)
    }

    val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)

    val seekTask = {
      import library._, MonoidSimple._
      import scala.concurrent.duration._
      theoremSearchTraceTask(dist1,
                             tv,
                             math.pow(10.0, -6),
                             3.minutes,
                             eqM(l)(r),
                             decay = 3)
    }

    val seek: Task[Option[Term]] = {
      import library._, MonoidSimple._
      import scala.concurrent.duration._
      theoremSearchTask(dist1,
                        tv,
                        math.pow(10.0, -6),
                        10.minutes,
                        eqM(l)(r),
                        decay = 3)
    }

    val seekFut: CancelableFuture[Option[Term]] = {
      proverDiv.appendChild(
        div(
          p("""The search has been started on the browser, which is very slow.
          You can query the server for much quicker computation.""")
        ).render
      )
      seek.runAsync
    }

    seekFut.foreach {
      case Some(t) =>
        val termDiv = div(style := "overflow-x: auto;")().render
        val typDiv  = div(style := "overflow-x: auto;")().render
        termDiv.innerHTML = katexSafe.renderToString(TeXTranslate(t))
        typDiv.innerHTML = katexSafe.renderToString(TeXTranslate(t.typ))
        proverDiv.appendChild(
          ul(`class` := "list-group")(
            li(`class` := "list-group-item list-group-item-primary")(
              "From the browser:"),
            li(`class` := "list-group-item list-group-item-info")("Theorem"),
            li(`class` := "list-group-item")(typDiv),
            li(`class` := "list-group-item list-group-item-success")("Proof"),
            li(`class` := "list-group-item")(termDiv),
          ).render)
      case None =>
        proverDiv.appendChild(h3("could not find proof of theorem").render)
    }

  }
}
