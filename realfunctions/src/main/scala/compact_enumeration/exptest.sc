package compact_enumeration

object exptest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
import spire.math._
import spire.algebra._
import spire.implicits._
import spire.syntax.literals._

  
  val apr = new ApproxTrig(200)                   //> apr  : compact_enumeration.ApproxTrig = compact_enumeration.ApproxTrig@5c21b
                                                  //| ae0
                                                  
  apr.width                                       //> res0: spire.math.Rational = 1/200
  
  apr.J                                           //> res1: spire.math.Interval[spire.math.Rational] = [0, 1/200]
 
	val e = apr.expstream                     //> e  : Stream[spire.math.Interval[spire.math.Rational]] = Stream([1], ?)

	e(500).mapBounds (_.toDouble)             //> res2: spire.math.Interval[Double] = [12.18236753530764, 12.18274729321148]
	
	e(250).mapBounds (_.toDouble)             //> res3: spire.math.Interval[Double] = [3.490324846673679, 3.490379247762552]
	
	exp(1)                                    //> res4: Double = 2.718281828459045
	
	
	apr.exp(1) mapBounds(_.toDouble)          //> res5: spire.math.Interval[Double] = [2.718270544696388, 2.718304438767226]
	
	apr.expDouble(1)                          //> res6: spire.math.Interval[Double] = [2.718270544696388, 2.718304438767226]
	
	apr.expDouble(1).upperBound               //> res7: spire.math.interval.Bound[Double] = Closed(2.718304438767226)
	
	ApproxTrig.getClosed(apr.expDouble(1).upperBound)
                                                  //> res8: Option[Double] = Some(2.718304438767226)
	
	exp(0.5)                                  //> res9: Double = 1.6487212707001282
}