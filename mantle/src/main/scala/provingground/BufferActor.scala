package provingground

import akka.actor._
import akka.stream._
import akka.stream.scaladsl.{Source => Src, _}
import akka.pattern.ask

import scala.concurrent._

import Hub.system

import akka.util.Timeout

 import scala.concurrent.ExecutionContext.Implicits.global

import BufferActor._

import scala.concurrent.duration._

class BufferActor[A] extends Actor{
  var buffer: Vector[A] = Vector()
  
  def receive = {
    case Save(a) => 
      buffer = buffer :+ (a.asInstanceOf[A])
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
  
  def sink() = Sink.foreach((a: A) => actor ! Save(a)) 
  
  implicit val timeout = Timeout(5.seconds)
  
  def query() = {
    val fut = (actor ? Query).map (_.asInstanceOf[Vector[A]])
    Await.result(fut, 5.seconds)
  }
    
  def flow[X] = Flow[X] mapConcat ((t) => query())
  
  def ticksource(d: FiniteDuration = 100.millis) = Src.tick(0.seconds, d, Tick : Ticker) 
  
  def source(d: FiniteDuration = 100.millis) = ticksource(d) via (flow[Ticker])
}