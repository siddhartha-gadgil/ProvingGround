package provingground

object Utils{
    import monix.eval

    import scala.collection.parallel.Task
    def bestTask[A](init: A, task: eval.Task[A], refine: eval.Task[A] => eval.Task[A]): eval.Task[A] =
        task.materialize.flatMap{
            t => t.fold((_) => eval.Task.now(init), (a) => bestTask(a, refine(task), refine))
        }
}