package provingground.andrewscurtis

import provingground._

import reactivemongo.api._
import reactivemongo.bson._

import upickle.default.{write => uwrite, read => uread, _}

import scala.concurrent.ExecutionContext.Implicits.global

import Moves._

import FreeGroups._

object ACMongo {
  lazy val driver = new MongoDriver()
  
  lazy val connection = driver.connection(List("localhost"))

  implicit lazy val db : DefaultDB = connection("provingground-andrewscurtis")
  
  lazy val elemsDB = db("elements")
  
  lazy val thmsDB = db("theorems")
  
  lazy val actorsDB = db("actors")
  
  implicit object ElemsWriter extends BSONDocumentWriter[ACElem]{
    def write(elem: ACElem) =
      BSONDocument(
        "name" -> elem.name,
        "moves" -> uwrite(elem.moves),
        "rank" -> elem.rank,
        "presentation" -> uwrite(elem.pres),
        "loops" -> elem.loops,
        "weight" -> elem.weight)
    
  }

  implicit object ElemsReader extends BSONDocumentReader[ACElem]{
    def read(doc: BSONDocument) = {
      val opt = 
        for (
            name <- doc.getAs[String]("name");
            pmoves <- doc.getAs[String]("moves");
            rank <- doc.getAs[Int]("rank");
            ppres <- doc.getAs[String]("presentation");
            loops <- doc.getAs[Int]("loops");
            weight <- doc.getAs[Double]("weight")
            )
        yield ACElem(
            name, 
            uread[Moves](pmoves), 
            rank, 
            uread[Presentation](ppres), 
            weight, loops)
      opt.get
    }          
  }
  
  implicit object ThmWriter extends BSONDocumentWriter[ACThm]{
    def write(elem: ACThm) =
      BSONDocument(
        "name" -> elem.name,
        "presentation" -> uwrite(elem.pres),
        "loops" -> elem.loops,
        "weight" -> elem.weight)    
  }
  
  implicit object ThmReader extends BSONDocumentReader[ACThm]{
    def read(doc: BSONDocument) = {
      val opt = 
        for (
            name <- doc.getAs[String]("name");
            ppres <- doc.getAs[String]("presentation");
            loops <- doc.getAs[Int]("loops");
            weight <- doc.getAs[Double]("weight")
            )
        yield ACThm(
            name,  
            uread[Presentation](ppres), 
            weight, loops)
      opt.get
    }
  }
  
  def addElem(el: ACElem) =  
    elemsDB.insert(el)
   
  def addThm(thm: ACThm) = 
    thmsDB.insert(thm)
 
  def updateLoops(name: String, loops: Int) = {
    val selector = BSONDocument("name" -> name)
   
    val init = BSONDocument("name" -> name, "loops" -> loops)
    
    val modifier = BSONDocument("$set" -> BSONDocument("loops" -> loops))
    
    val checkCursor = actorsDB.find(selector).cursor[BSONDocument]()
    val emptyFut = checkCursor.headOption map (_.isEmpty)
    
    emptyFut.foreach ((check) =>
      if (check) actorsDB.insert(init) else actorsDB.update(selector, modifier)
      )
  }
}