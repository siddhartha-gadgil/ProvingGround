package provingground.andrewscurtis

import provingground._

import reactivemongo.api._
import reactivemongo.bson._

import reactivemongo.api.commands.WriteResult

import upickle.default.{write => uwrite, read => uread, _}

import scala.concurrent.ExecutionContext.Implicits.global

import Moves._

import FreeGroups._

import scala.concurrent._

import Collections._

object ACMongo extends ACWriter{
  lazy val driver = new MongoDriver()
  
  lazy val connection = driver.connection(List("localhost"))

  implicit lazy val db : DefaultDB = connection("provingground-andrewscurtis")
  
  lazy val elemsDB = db("elements")
  
  lazy val thmsDB = db("theorems")
  
  lazy val actorsDB = db("actors")
  
  lazy val moveWeightsDB = db("moveweights")
  
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
  
  
  implicit object MoveWeightWriter extends BSONDocumentWriter[ACMoveWeights]{
    def write(elem: ACMoveWeights) = 
      BSONDocument(
          "name" -> elem.name,
          "fdM" -> uwrite(elem.fdM),
          "loops" -> elem.loops
)
  }
  
  implicit object MoveWeightReader extends BSONDocumentReader[ACMoveWeights]{
    def read(doc: BSONDocument) = {
      val opt = 
        for (
            name <- doc.getAs[String]("name");
            pfdM <- doc.getAs[String]("fdM");
            loops <- doc.getAs[Int]("loops")
            )
        yield ACMoveWeights(
            name,  
            uread[FiniteDistribution[AtomicMove]](pfdM), 
            loops)
      opt.get
    }
  }
  
  def addElem(el: ACElem) =  
    elemsDB.insert(el)
   
  def addThm(thm: ACThm) = 
    thmsDB.insert(thm)
 
  def addMoveWeight(wts: ACMoveWeights) =
    moveWeightsDB.insert(wts)
    
  def updateLoops(name: String, loops: Int) = {
    val selector = BSONDocument("name" -> name)
   
    val init = BSONDocument("name" -> name, "loops" -> loops)
    
    val modifier = BSONDocument("$set" -> BSONDocument("loops" -> loops))
    
    val checkCursor = actorsDB.find(selector).cursor[BSONDocument]()
    val emptyFut = checkCursor.headOption map (_.isEmpty)
    
    emptyFut map ((check) =>
      if (check) actorsDB.insert(init) else actorsDB.update(selector, modifier)
      )
  }
  
  def getFutElemsStep(name: String, loops: Int) = 
  {
    val selector = BSONDocument("name" -> name, "loops" -> loops)
    elemsDB.find(selector).cursor[ACElem]().collect[Vector]()
  }
  
  def getFutThmElemsStep(name: String, loops: Int) = 
  {
    val selector = BSONDocument("name" -> name, "loops" -> loops)
    thmsDB.find(selector).cursor[ACThm]().collect[Vector]()
  }
  
  def getFutOptFDMStep(name: String, loops: Int) = 
  {
    val selector = BSONDocument("name" -> name, "loops" -> loops)
    moveWeightsDB.find(selector).cursor[ACMoveWeights]().headOption map ((opt) => opt map (_.fdM))
  }
  
  def getFutOptLoops(name: String) = {
    val selector = BSONDocument("name" -> name)
    val futOpt = actorsDB.find(selector).cursor[BSONDocument]().headOption
    futOpt map ((opt) =>
      (opt) flatMap (
          (doc) => doc.getAs[Int]("loops"))
      )
  }
  
  def getFutOptElems(name: String) = 
    getFutOptLoops(name) flatMap (
          {
            case Some(loops) => getFutElemsStep(name, loops) map ((vec) => Some(vec))
            case None => Future.successful(None)
          }
        )
        
  def getFutOptThmElems(name: String) = 
    getFutOptLoops(name) flatMap (
          {
            case Some(loops) => getFutThmElemsStep(name, loops) map ((vec) => Some(vec))
            case None => Future.successful(None)
          }
        )

  def getFutOptFDM(name: String) = 
    getFutOptLoops(name) flatMap (
          {
            case Some(loops) => getFutOptFDMStep(name, loops) 
            case None => Future.successful(None)
          }
        )

        
        
  def  mapFutOpt[S, T](futOpt : Future[Option[S]])(fn: S => T) = {
    futOpt map ((opt) => opt map (fn))
  }

  def collect[T](xs: Seq[Future[Option[T]]]) = 
    Future.sequence(xs) map (_.flatten)
  
  def getFutOptFDV(name: String) =
    mapFutOpt(getFutOptElems(name))(
        (vec: Vector[ACElem]) => 
          FiniteDistribution(
            vec map ((elem) => Weighted(elem.moves, elem.weight))
          )
  )
  
  def getFutOptThms(name: String) =
    mapFutOpt(getFutOptThmElems(name))(
        (vec: Vector[ACThm]) => 
          FiniteDistribution(
            vec map ((elem) => Weighted(elem.pres, elem.weight))
          )
  )
        
  def getFutOptState(name: String) = 
    for (optFDM <- getFutOptFDM(name); optFDV <- getFutOptFDV(name)) 
      yield {(optFDM, optFDV) match {
        case (Some(fdM), Some(fdV)) => Some((fdM, fdV))
        case _ => None
    }
    }
}


case class ACMoveWeights(name: String, fdM : FiniteDistribution[AtomicMove], loops: Int)
    