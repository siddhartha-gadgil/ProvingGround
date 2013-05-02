package provingGround

object Aware{
  
	trait MemoryBank[A]{
		var memories: List[A] =List()
		
		def remember(a: A) = {memories = a :: memories}
		
		def clear = {memories = List()}
		
		def filter(f: A => Boolean) = {memories = (memories filter (f))}
	}
	
	def remember[A](a: A)(implicit mem: MemoryBank[A]) = mem.remember(a)

	def recallAll[A](implicit mem:MemoryBank[A]) = mem.memories
	
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

	
	
	trait Resource
	
	trait ForTask{
	  def canUse(task: Task): Boolean
	}

	trait OptionResource[A] extends Resource{
		def apply(task: Task): Option[A]
		}

	trait StreamResource[A] extends Resource{
		def apply(task: Task): Stream[A]
		def getList(task: Task, n: Int) = (apply(task) take n).toList 
		}
	
	trait MemResource[A] extends Resource{
		def apply(): List[A]
		}
	
	class Recall[A](implicit mem: MemoryBank[A]) extends MemResource[A]{
		def apply() = mem.memories
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
