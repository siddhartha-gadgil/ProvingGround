package provingground.interface

import ujson.Js
import upickle.default._
import monix.eval._

import scala.util.{Failure, Success}

case class MultiTask(jobs: Map[String, String => Task[String]])
    extends (String => Task[String]) {
  def apply(inp: String): Task[String] = {
    val obj: ujson.Obj = ujson.read(inp).obj
    val jobName        = obj("job").str
    scribe.info(s"received job request $jobName")
    val job: String => Task[String] = jobs.getOrElse(
      jobName,
      (_) =>
        Task.raiseError(
          new IllegalArgumentException(s"Cannot find job with name $jobName")))
    val data = ujson.write(obj("data"))
    job(data).materialize.map {
      case Success(result: String) =>
        scribe.info(s"completed job $jobName")
        ujson.write(
          ujson.Obj("job"     -> ujson.Str(jobName),
                    "data"    -> ujson.Str(data),
                    "result"  -> ujson.Str(result),
                    "success" -> ujson.Bool(true))
        )
      case Failure(err: Throwable) =>
        scribe.error(s"failed job $jobName")
        ujson.write(
          ujson.Obj("job"     -> ujson.Str(jobName),
                    "data"    -> ujson.Str(data),
                    "error"   -> ujson.Str(err.getMessage),
                    "success" -> ujson.Bool(false))
        )
    }
  }

  def +(kv: (String, String => Task[String])): MultiTask = MultiTask(jobs + kv)

  def ++(that: MultiTask): MultiTask = MultiTask(jobs ++ that.jobs)
}

object MultiTask {
  def apply(kvs: (String, String => Task[String])*): MultiTask =
    new MultiTask(kvs.toMap)
}
