package provingground

object Utils{
    import monix.eval._

    def refinedTask[A](init: A, task: Task[A], refine: Task[A] => Task[A]): Task[A] =
        task.materialize.flatMap{
            t => t.fold((_) => Task.now(init), (a) => refinedTask(a, refine(task), refine))
        }
    
    def bestTask[A](taskSeq: Seq[Task[A]], accum: Option[A] = None): Task[Option[A]] = 
            taskSeq.headOption.map(_.materialize.flatMap(
                t => t.fold((_) => Task.now (accum), a => bestTask(taskSeq.tail, Some(a)))
            )).getOrElse(Task.now(accum))
            
        
}