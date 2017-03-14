package provingground
import scala.util._

/** Converts prose numbers to Integers without checking for bad cases */
object TextToInt {

  private val wordNumber: Map[String, Long] = Map(
      "one" -> 1,
      "two" -> 2,
      "three" -> 3,
      "four" -> 4,
      "five" -> 5,
      "six" -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine" -> 9,
      "ten" -> 10,
      "eleven" -> 11,
      "twelve" -> 12,
      "thirteen" -> 13,
      "fourteen" -> 14,
      "fifteen" -> 15,
      "sixteen" -> 16,
      "seventeen" -> 17,
      "eighteen" -> 18,
      "nineteen" -> 19,
      "twenty" -> 20,
      "thirty" -> 30,
      "forty" -> 40,
      "fifty" -> 50,
      "sixty" -> 60,
      "seventy" -> 70,
      "eighty" -> 80,
      "ninety" -> 90,
      "hundred" -> 100,
      "thousand" -> 1000,
      "lakh" -> 100000,
      "million" -> 1000000,
      "crore" -> 10000000,
      "billion" -> 1000000000,
      "and" -> 0
  )

  object Long {
    def unapply(s: String): Option[Long] = Try(s.toLong).toOption
  }

  object Int {
    def unapply(s: String): Option[Int] = Try(s.toInt).toOption
  }

  /** Returns a number for a single word*/
  def wordNum(w: String) = w match {
    case Long(x) => x
    case _ => wordNumber(w.toLowerCase)
  }

  /** Returns a number for a list of words */
  def wordListNumber(l: List[String]): Long = numListNumber(l map wordNum)

  private def numListNumber(nums: List[Long]): Long =
    if (nums.isEmpty) 0
    else {
      val maxNum = nums.max
      val splitPos = nums.indexOf(maxNum)
      if (maxNum < 100) nums.sum
      else {
        val headList = nums take splitPos
        val tailList = nums drop (splitPos + 1)
        if (headList.isEmpty) maxNum + numListNumber(tailList)
        else {
          numListNumber(headList) * maxNum + numListNumber(tailList)
        }
      }
    }

  /** Splits a string into words*/
  def splitString(t: String): List[String] = t.split("[ ,.]+").toList

  /** Returns a number from a string of many words */
  def stringNumber(t: String): Long = wordListNumber(splitString(t))
}
