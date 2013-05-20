package provingGround

import akka.actor._

object Aware{
  

	trait Task{
	  val goal: Task = this match{
	    case TaskUsing(task, _) => task.goal
	    case BoundedTask(task, _) => task.goal
	    case t: Task => t
	  }
	}
	
	
	trait DeterminateTask extends Task
	
	trait OpenEndedTask extends Task


	trait Response{
	  val goal: Task
	}
	
	trait Result[+A]{
	  val result: A
	}
	
	case class TaskUsing[A](task: Task, using: Result[A]) extends Task
	
	case class BoundedTask(task: Task, bound: Long) extends Task
	
	case class Results(result: List[Result[Any]]) extends Result[List[Result[Any]]]
	
	case class Solution[+A](result: A, goal: Task) extends Response with Result[A]
	
	case class PartialSolution[+A, +B](result: A, goal: Task, persist: B = Nil) extends Response with Result[A]
	
	case class Reduction[A, B](reduction: Task, goal: Task, 
	    solver: Solution[A] => Solution[B]) extends Response with Result[(Task, Solution[A] => Solution[B])]{
	  val result = (reduction, solver)
	}
	
	case class Ask(task: Task, goal: Task) extends Response
	
	case class Success(goal: Task) extends Response
	
	case class Timedout(task: Task)
	
	case object AllUpdates
	
	case class Updates(task: Task)

	case class Acknowledgement[A](solution: Solution[A])

	case class Query(task: Task, asker: ActorRef)
	
	case class Failed(task: Task)
	
	case class Continue(task: Task)

	
	
	
trait Attention[A]{
		val subject: A
	}

	trait Critic[A,B]{
		def apply(body: A, context: B): Long
	}
	
	
	
	
	trait SimpleResource{
	  val resource: PartialFunction[Task, Response]
	  def apply(t: Task): Option[Response] = resource.lift(t)
	}
	 
	case class SeekerResponse(response: Response, nextiter: SeekerResource)
	
	case class NotFound(goal: Task) extends Response
	
	trait SeekerResource{
	  def resource: PartialFunction[Task, Either[SeekerResponse, Response]]
	  def receiver(asker: ActorRef, cntx: ActorContext, myself: ActorRef): PartialFunction[Any, Unit] =  {
	    case task: Task =>
	      resource(task) match {
	        case Right(resp: Response) => 
	          asker ! resp
	          cntx.stop(myself)
	        case Left(SeekerResponse(response, nextiter)) =>
	          asker ! response
	          cntx.become(nextiter.receiver(asker, cntx, myself))
	      }
	  }
	}
	
	object SeekerResource{
	  def fromSeek[A](seek: (Long, Set[A]) => Either[Set[A], A], init: Set[A]) = {
	  } 
	  
	  def fromStream[A](s: Stream[A]): SeekerResource = new SeekerResource {
	    val resource: PartialFunction[Task, Either[SeekerResponse, Response]] = {
	      case FindInStream(seeker , p, n: Int) if seeker==s => 
	        val task = FindInStream(s , p, n)
	        ((s take n).find(p) map ((a: A) => Right(Solution(a, task)))).getOrElse(Left(SeekerResponse(NotFound(task), fromStream(s drop n))))
	    }	    	    
	  }
	  
	  def fromSeeker[A, S](s: Seeker[A, S]): SeekerResource = fromSeeker(s, s.base)
	  
	  def fromSeeker[A, S](s: Seeker[A, S], init: S): SeekerResource = new SeekerResource {
	    val resource: PartialFunction[Task, Either[SeekerResponse, Response]] = {
	      case Seek(seeker , p, n: Long) if seeker == s => 
	        val task = Seek(s , p, n: Long)
	        s.seek(p, n, init) match {
	          case Right(a) =>  Right(Solution(a, task))
	          case Left(state) => Left(SeekerResponse(s.noluck(task)(state), fromSeeker(s, state)))
	        }
	    }
	  }
	}
	
	trait Seeker[A, S]{
	  def seek(p: A => Boolean, n: Long, init: S): Either[S, A]
	  
	  val base: S
	  
	  def noluck(task: Task)(s: S): Response = NotFound(task)
	}
	
	case class Seek[A, S](s: Seeker[A, S], p: A => Boolean, n: Long) extends Task
	
	
	case class FindInStream[A](s: Stream[A], p: A => Boolean, n: Int) extends Task
		
	trait StreamResource{
	  def resource: PartialFunction[Task, Long => Response]
	}
	
	class TaskMaster extends Actor{
	  def receive = {
	    case _ => 
	  }
	}
	
	class SimpleResourceActor(res: SimpleResource) extends Actor{
	  def receive = {
	    case task: Task => res.resource.lift(task) foreach (sender ! _)
	  }
	}
	
	class SeekerResourceRouter(iter: SeekerResource, cycle: Long = 1000) extends Actor{
	  var tasks: Set[Query] = Set.empty
	  var partialSolutions: Set[PartialSolution[Any, Any]] = Set.empty
	  var solutions: Set[Solution[Any]] = Set.empty
	  var taskActors: Map[Task, ActorRef]= Map.empty
	  var Cycles: Map[ActorRef, Long] = Map.empty
	  
	  def receive = {
	    case Seek(it, p, n) =>
	      val task = Seek(it, p, n)
	      tasks += Query(task, sender)
	      val actorRef = (new SeekerResourceSeeker(iter)).self
	      taskActors = taskActors ++ Map(task -> actorRef)
	      Cycles = Cycles ++ Map(actorRef -> n)
	      actorRef ! Seek(it, p, cycle)
	    case _ : Task => 
	    case Solution(a, task) =>
	      solutions += Solution(a, task)
	      for (Query(`task`, asker) <- tasks) asker ! Solution(a, task)
	    case NotFound(Seek(it, p, _)) =>
	      val cycles = Cycles(sender) - cycle
	      if (cycles>0) {
	        Cycles = Cycles ++ Map(sender -> cycles) 
	      	sender ! Seek(it, p, Math.min(cycle, cycles))
	      }
	      else for (Query(Seek(`it`, `p`, n), asker) <- tasks) asker ! NotFound(Seek(it, p, n))
	  }
	}
	
	class SeekerResourceSeeker(iter: SeekerResource) extends Actor{
	  def receive = iter.receiver(sender, context, self)  
	}
	
	trait Generator[A, B]{
	  def apply(n: Long, b: B): GeneratorOut[A, B]
	  
	  def receiver(asker: ActorRef, cntx: ActorContext): PartialFunction[Any, Unit] = {
	    case Generate(genn, n, b) if this == genn => 
	      val out = genn(n, b)
	      asker ! Solution(out.out, Generate(genn, n, b ))
	      cntx.become(out.newGen.receiver(asker, cntx))
	  }
	}
	
	case class GeneratorOut[A, B](out: Set[A], newGen : Generator[A,B])
	
	case class Generate[A, B](gen: Generator[A, B], n: Long, init: B) extends Task
	
	class GeneratorActor[A,  B](gen: Generator[A, B]) extends Actor{
	  def receive = gen.receiver(sender, context) 
	  }
	
	
	class GeneratorRouter[A, B](gen: Generator[A, B]) extends Actor{
	  var tasks: Set[Query] = Set.empty
	  var taskActors: Map[Task, ActorRef] = Map.empty
	  
	  def receive = {
	    case Generate(genn, n, b) if gen == genn =>
	      val task = Generate(genn, n, b)
	      tasks += Query(task, sender)
	      val actorRef = (new GeneratorActor(gen)).self
	      taskActors = taskActors ++ Map(task -> actorRef)
	      actorRef ! Generate(genn, n, b)
	    case _ : Task => 
	    case Solution(a, task) =>
	      for (Query(`task`, asker) <- tasks) asker ! Solution(a, task)
	  }
	}
	
	
	trait Description[A]{
		val subject: A
	}

	trait Value[A] extends Description[A]

	case class StringDescription[A](subject: A, description: String) extends Description[A]
	
	trait Relation[A,B]{
		val first: A
		val second: B
	}
	
	case class NamedRelation[A,B](first: A, second: B, name: String) extends Relation[A,B]

	}
