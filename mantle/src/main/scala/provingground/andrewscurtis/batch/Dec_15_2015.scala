package provingground.andrewscurtis.batch

import provingground._

import andrewscurtis._

object Dec_15_2015 {
  val mickey = StartData("Mickey")
  
  val donald = StartData("Donald", wrdCntn = 0.05, steps = 4)
  
  val goofy = StartData("Goofy", steps = 6, strictness = 2.5)
  
  val pluto = StartData("Pluto", steps = 8, wrdCntn = 0.03)
}