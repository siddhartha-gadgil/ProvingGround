package provingground.interface
import provingground._, library._
import scala.util._, Properties.envOrNone
import Utils._, JvmUtils._
import scribe.output.format.ASCIIOutputFormat

object CzSlRun extends App {

  logger.info(
    """|This is a Bot based run for the Czech-Slovak olympiand problem identified by Achal, who proved this with manual choice.
                 |This problem involves showing the left and right identity coincide given a bunch of axioms. We can only use forward reasoning
                 |since there are no models.
                 |
                 |There is a little domain knowledge in the tuning, with tautologies specified (to not be used as lemmas) 
                 |and "base states" given based on what is used together. But this is largely a robust choice of parameters.
                 |Much of the work has been in ensuring that all the hot code is parallelized.
                 |""".stripMargin
  )

  

  envOrNone("JAVA_OPTS").foreach(w => logger.info(s"JAVA_OPTS: $w"))

  envOrNone("JAVA_HOME").foreach(w => logger.info(s"JAVA_HOME: $w"))

  CzSlOly.sessF

  while (Utils.running) {
    Thread.sleep(100)
  }

}
