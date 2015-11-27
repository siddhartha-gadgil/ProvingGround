package provingground

import andrewscurtis._

import ACRoutes._
import ACFlow.{system, mat}

import akka.http._
import akka.http.scaladsl._
import akka.http.scaladsl.server.Directives._

import scala.concurrent.ExecutionContext.Implicits.global

import scala.io.StdIn

object HttpServer extends App {
  val server = new ServerStart
  
  val bindingFuture = server.bindingFuture
 
  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine() // for the future transformation 
  server.stop  
}

class ServerStart{
  val route = acRoutes ~ getFromResourceDirectory("")
  
  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
  
  def stop() = 
    bindingFuture.
    flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ ⇒ FDhub.stop(ACBatch.quickhub)) // and shutdown when done
}