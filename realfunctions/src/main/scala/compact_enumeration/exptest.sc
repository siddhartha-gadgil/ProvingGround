package compact_enumeration

object exptest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import spire.math._
import spire.algebra._
import spire.implicits._
import spire.syntax.literals._

  
  val apr = new ApproxTrig(500)                   //> apr  : compact_enumeration.ApproxTrig = compact_enumeration.ApproxTrig@1da47
                                                  //| 7bf
                                                  
  apr.width                                       //> res0: spire.math.Rational = 1/500
  
  apr.J                                           //> res1: spire.math.Interval[spire.math.Rational] = [0, 1/500]
 
	val e = apr.expstream                     //> e  : Stream[spire.math.Interval[spire.math.Rational]] = Stream([1], ?)

	e(500).mapBounds (_.toDouble)             //> res2: spire.math.Interval[Double] = [2.718280018987869, 2.718285450127635]
	
	e(250).mapBounds (_.toDouble)             //> res3: spire.math.Interval[Double] = [1.6487207219501636, 1.6487223690262818]
                                                  //| 
	
	exp(1)                                    //> res4: Double = 2.718281828459045
	
	exp(0.5)                                  //> res5: Double = 1.6487212707001282
}