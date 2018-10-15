package provingground.interface

import upickle.Js
import upickle.default._
import monix.eval._

import scala.util.{Failure, Success}

case class MultiTask(jobs: Map[String, String => Task[String]]) extends (String => Task[String]){
  def apply(inp: String) : Task[String] = {
    val obj: Js.Obj = read[Js.Obj](inp)
    val jobName =  obj("job").str
    val job: String => Task[String] = jobs.getOrElse(jobName, (_) => Task.raiseError(new IllegalArgumentException(s"Cannot find job with name $jobName")))
    val data = obj("data").str
    job(data).materialize.map{
      case Success(result: String) =>
        write[Js.Obj](
          Js.Obj("job" -> Js.Str(jobName), "data" -> Js.Str(data), "result" -> Js.Str(result), "success" -> Js.Bool(true))
        )
      case Failure(err: Throwable) =>
        write[Js.Obj](
          Js.Obj("job" -> Js.Str(jobName), "data" -> Js.Str(data), "error" -> Js.Str(err.toString), "success" -> Js.Bool(false))
        )
    }
  }

  def +(kv: (String, String => Task[String])): MultiTask = MultiTask(jobs + kv)

  def ++(that: MultiTask): MultiTask = MultiTask(jobs ++ that.jobs)
}

object MultiTask{
  def apply(kvs: (String, String => Task[String])* ): MultiTask = new MultiTask(kvs.toMap)
}