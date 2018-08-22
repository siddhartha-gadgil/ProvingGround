package provingground.interface

import scala.concurrent.ExecutionContext.Implicits.global
import MantleService._

object MantleRoutes extends cask.Routes{
  @cask.staticFiles("docs")
  def docsRoute() = "docs"

  @cask.staticResources("resources")
  def public() = "."

  @cask.get("/")
  def root() = Site.page(indexHTML,
            "resources/",
            "ProvingGround HoTT Server",
            false)

  @cask.get("/index.html")
  def index() = Site.page(indexHTML,
            "resources/",
            "ProvingGround HoTT Server",
            false)

  @cask.get("/prover.html")
  def prover() = Site.page(proverHTML,
            "resources/",
            "ProvingGround HoTT Server",
            false)

  @cask.post("/monoid-proof")
  def seek() = {
    pprint.log("seeking proof")
    val pfFut = MonoidServer.seekResultFut
    pfFut.foreach((pf) => pprint.log(pf))
    "seeking proof"
  }

  @cask.route("build", methods = Seq("get", "post"))
  def site(request: cask.Request) =
    trySite()

  @cask.get("/scripts/index.html")
  def fiddle() = AmmService.indexHTML

  @cask.post("/scripts/kernel")
  def repl(request: cask.Request) =
    {
      val d = new String(request.readAllBytes())
      println(s"post received:\n$d")

      val result =
        AmmService.replResult(d) match {
          case Right(z) => "--RESULT--\n" + z
          case Left(z)  => "--ERROR--\n" + z
        }
    }

  initialize()

}

object MantleCask extends cask.Main(MantleRoutes)
