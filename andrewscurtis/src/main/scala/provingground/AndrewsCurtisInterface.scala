package provingground.andrewscurtis

import AndrewsCurtis._
import provingground._, learning._, Collections._
import FreeGroups._
//import play.api.libs.json._
//import play.api.libs.concurrent._
import akka.actor._
//import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent._

object AndrewsCurtisInterface {
  /*
  def sendDstbn(dstbn : FiniteDistribution[Presentation]) = dstbnChannel.push(dstbnJson(dstbn))

  def pushDstbn(d : DynDstbn) = sendDstbn(d.vrtdst)

  /*
 * sending out distributions on presentations to frontend : the dstbnout Enumerator is sent in response to an sse request
 *
 */

  val (dstbnout, dstbnChannel) = Concurrent.broadcast[JsValue]

    class FlowController(initParams: ACparameters, worker : ActorRef, output : DynDstbn => Unit = pushDstbn) extends Actor{
    var params = initParams

    var dstbn : DynDstbn = baseDstbn

    def receive = {
      case p : ACparameters =>
        params = p
      case d : DynDstbn =>
        dstbn = d
        output(d)
        worker ! ACFlowData(params, d)
      case _ =>
    }
  }

  object FlowController{
    def props(initParams: ACparameters, output : DynDstbn => Unit = pushDstbn) = Props(new FlowController(initParams, flowWorker, output))
  }

  class FlowWorker extends Actor{
    def receive = {
      case ACFlowData(p, d) => sender() ! p.flow(d)
      case _ =>
    }
  }

  val flowWorker = Akka.system.actorOf(Props[FlowWorker])

  val flowController = FlowController.props(ACparameters())

 */
}
