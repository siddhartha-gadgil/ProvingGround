package provingground
// import example._

import akka.http.scaladsl.server.Directives
// import shared.SharedMessages

class WebService() extends Directives {

  val route = {
    pathSingleSlash {
      get {
        complete {
          provingground.html.index.render(provingground.HoTT.Type.toString)
        }
      }
    } ~
      pathPrefix("assets" / Remaining) { file =>
        // optionally compresses the response with Gzip or Deflate
        // if the client accepts compressed responses
        encodeResponse {
          getFromResource("public/" + file)
        }
      }
  }
}
