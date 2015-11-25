package provingground

import akka.actor._
import akka.stream._
import akka.stream.scaladsl.{Source => Src, _}
import akka.pattern.ask

import Hub.system

import akka.util.Timeout

 import scala.concurrent.ExecutionContext.Implicits.global

import BufferActor._

import scala.concurrent.duration._

class BufferActor[A] extends Actor{
  var buffer: Vector[A] = Vector()
  
  def receive = {
    case Save(a) => 
      buffer :+ (a.asInstanceOf[A])
    case Query =>
      sender ! buffer
      buffer = Vector()
  }
}


object BufferActor{
  case class Save[A](a: A)
  
  case object Query
  
  class Ticker
  
  case object Tick extends Ticker
}

class BufferFlow[A]{
  def props = Props(new BufferActor[A])
  
  val actor = system.actorOf(props)
  
  def sink = Sink.foreach((a: A) => actor ! Save(a)) 
  
  implicit val timeout = Timeout(5.seconds)
  
  def query = (actor ? Query).map (_.asInstanceOf[A])
  
  val flt = Flow[Ticker]
  
  def flow = (flt mapAsync[A] _)(2)((t: Ticker) => query)
}