package provingGround

object Aware{

	trait Task

	trait Goal extends Task
	
	trait DeterminateTask extends Task
	
	trait OpenEndedTask extends Task

	trait Attention[A]{
		val subject: A
	}

	trait Promise{
		val task: Task
	}
  
	class Contributes[A](val body: A, val task: Task) 

	class Achieves[A](body: A, task: Task) extends Contributes[A](body, task)

	trait Critic[A,B]{
		def apply(body: A, context: B): Int
	}

	trait Resource{
		def forTask(task: Task): Boolean
	}

	trait OptionResource[A] extends Resource{
		def apply(task: Task): Option[A]
		}

	trait StreamResource[A] extends Resource{
		def apply(task: Task): Stream[A]
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
