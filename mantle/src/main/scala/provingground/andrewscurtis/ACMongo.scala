package provingground.andrewscurtis

import provingground._

import reactivemongo.api._
import reactivemongo.bson._

import reactivemongo.api.commands.WriteResult

import reactivemongo.api.indexes._

import upickle.default.{write => uwrite, read => uread, _}

import scala.concurrent.ExecutionContext.Implicits.global

import Moves._

import FreeGroups._

import scala.concurrent._

import Collections._

import MoveGenerator._

object ACMongo extends ACWriter{
  lazy val driver = new MongoDriver()
  
  lazy val connection = driver.connection(List("localhost"))

  /**
   * The database for andrews-curtis runs.
   */
  implicit lazy val db : DefaultDB = connection("provingground-andrewscurtis")
  
  /**
   * Collection for andrews-curtis elements - elements have all the info.
   */
  lazy val elemsDB = db("elements")
  
  val elemsInd = elemsDB.indexesManager
  
  val index = new Index(Seq("name" -> IndexType.Ascending, "loops" -> IndexType.Descending))
  
  elemsInd.ensure(index) // index elements by actor name and loops (descending).
  
  /**
   * collections of theorems, with total weights.
   */
  lazy val thmsDB = db("theorems")
  
  val thmsInd = thmsDB.indexesManager
  
  thmsInd.ensure(index) // index theoreme by actor name and loops (descending)
  
  val thmsPresIndex = new Index(Seq("presentation" -> IndexType.Hashed))
  
  thmsInd.ensure(thmsPresIndex) // also index by hhashed presentation
  
  /**
   * collection for actor data: names, loops run and anything else stored.
   */
  lazy val actorsDB = db("actors") 
  
  /**
   * collection for weights of moves.
   */
  lazy val moveWeightsDB = db("moveweights")
  
  /**
   * implicit writer for AC elements
   */
  implicit object ElemsWriter extends BSONDocumentWriter[ACElem]{
    def write(elem: ACElem) =
      BSONDocument(
        "name" -> elem.name,
        "moves" -> uwrite(elem.moves), // pickled form of moves
        "rank" -> elem.rank,
        "presentation" -> uwrite(elem.pres), // pickled form of presentation
        "loops" -> elem.loops,
        "weight" -> elem.weight)
    
  }

  /**
   * implicit reader for andrews-curtis elements
   */
  implicit object ElemsReader extends BSONDocumentReader[ACElem]{
    def read(doc: BSONDocument) = {
      val opt = 
        for (
            name <- doc.getAs[String]("name");
            pmoves <- doc.getAs[String]("moves"); // pickled form of  moves
            rank <- doc.getAs[Int]("rank");
            ppres <- doc.getAs[String]("presentation"); // picked form of presentation
            loops <- doc.getAs[Int]("loops");
            weight <- doc.getAs[Double]("weight")
            )
        yield ACElem(
            name, 
            uread[Moves](pmoves), //unpickle moves
            rank, 
            uread[Presentation](ppres), //unpickle presentation 
            weight, loops)
      opt.get
    }          
  }
  
  /**
   * implicit writer for andrews-curtis theorems
   */
  implicit object ThmWriter extends BSONDocumentWriter[ACThm]{
    def write(elem: ACThm) =
      BSONDocument(
        "name" -> elem.name,
        "presentation" -> uwrite(elem.pres), // pickled presentation
        "loops" -> elem.loops,
        "weight" -> elem.weight)    
  }
  
  /**
   * implicit reader for andrews-curtis theorems
   */
  implicit object ThmReader extends BSONDocumentReader[ACThm]{
    def read(doc: BSONDocument) = {
      val opt = 
        for (
            name <- doc.getAs[String]("name");
            ppres <- doc.getAs[String]("presentation"); // pickle presentation
            loops <- doc.getAs[Int]("loops");
            weight <- doc.getAs[Double]("weight")
            )
        yield ACThm(
            name,  
            uread[Presentation](ppres), // unpickle presentation 
            weight, loops)
      opt.get
    }
  }
  
  /**
   * implicit writer for moves
   */
  implicit object MoveWeightWriter extends BSONDocumentWriter[ACMoveWeights]{
    def write(elem: ACMoveWeights) = 
      BSONDocument(
          "name" -> elem.name,
          "fdM" -> uwrite(elem.fdM),
          "loops" -> elem.loops
)
  }

  /**
   * implicit reader for moves
   */
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
  
  /**
   * insert element in AC element collection
   */
  def addElem(el: ACElem) =  
    elemsDB.insert(el)
  
    /**
     * insert theorem in collection
     */
  def addThm(thm: ACThm) = 
    thmsDB.insert(thm)
 
  /**
   * insert move weights in collection.
   */
  def addMoveWeight(wts: ACMoveWeights) =
    moveWeightsDB.insert(wts)
  
  /**
   * update number of loops of an actor;
   *   if there is other actor data, this is left unchanged.
   */
  def updateLoops(name: String, loops: Int) = {
    val selector = BSONDocument("name" -> name)
   
    val init = BSONDocument("name" -> name, "loops" -> loops) //initial data if no entry exists
    
    val modifier = BSONDocument("$set" -> BSONDocument("loops" -> loops)) //update 
    
    val checkCursor = actorsDB.find(selector).cursor[BSONDocument]()
    
    val emptyFut = checkCursor.headOption map (_.isEmpty) // check if entry for actor exists.
    
    emptyFut map ((check) =>
      if (check) actorsDB.insert(init) else actorsDB.update(selector, modifier)
      )
  }
  
  /**
   * get elements as future given actor name and stage.
   */
  def getFutElemsStep(name: String, loops: Int) = 
  {
    val selector = BSONDocument("name" -> name, "loops" -> loops)
    elemsDB.find(selector).cursor[ACElem]().collect[Vector]()
  }
  
  /**
   * get theorems given actor name and loops as future.
   */
  def getFutThmElemsStep(name: String, loops: Int) = 
  {
    val selector = BSONDocument("name" -> name, "loops" -> loops)
    thmsDB.find(selector).cursor[ACThm]().collect[Vector]()
  }
  
  /**
   * get (optional, future) weights of moves given actor name and loops. 
   */
  def getFutOptFDMStep(name: String, loops: Int) = 
  {
    val selector = BSONDocument("name" -> name, "loops" -> loops)
    moveWeightsDB.find(selector).cursor[ACMoveWeights]().headOption map ((opt) => opt map (_.fdM))
  }
  
  /**
   * get the number of loops run by an actor as an optional future.
   */
  def getFutOptLoops(name: String) = {
    val selector = BSONDocument("name" -> name)
    val futOpt = actorsDB.find(selector).cursor[BSONDocument]().headOption
    futOpt map ((opt) =>
      (opt) flatMap (
          (doc) => doc.getAs[Int]("loops"))
      )
  }
  
  /**
   * get elements given only actor name (as future, option)
   */
  def getFutOptElems(name: String) = 
    getFutOptLoops(name) flatMap (
          {
            case Some(loops) => getFutElemsStep(name, loops) map ((vec) => Some(vec))
            case None => Future.successful(None)
          }
        )
  /**
   * get theorems given only actor name (as future, option)      
   */
  def getFutOptThmElems(name: String) = 
    getFutOptLoops(name) flatMap (
          {
            case Some(loops) => getFutThmElemsStep(name, loops) map ((vec) => Some(vec))
            case None => Future.successful(None)
          }
        )

  /**
   * get weights of atomic moves given actor name (as future, option)
   */
  def getFutOptFDM(name: String) = 
    getFutOptLoops(name) flatMap (
          {
            case Some(loops) => getFutOptFDMStep(name, loops) 
            case None => Future.successful(None)
          }
        )

        
  /**
   * mapping future options,
   * should upgrade to monadic implicit class.      
   */
  def  mapFutOpt[S, T](futOpt : Future[Option[S]])(fn: S => T) = {
    futOpt map ((opt) => opt map (fn))
  }

  /**
   * monadic operations for future options
   */
  implicit class FutOptMonad[A](futOpt : Future[Option[A]]){
    def mapp[B](fn: A => B) = futOpt map ((opt) => opt map (fn))
    
    def flatMapp[B](fn: A => Future[Option[B]]) = {
      futOpt flatMap {
        case Some(b) => fn(b)
        case None => Future.successful(None)
      }
    }
  }
  
  /**
   * collect on future options
   */
  def collect[T](xs: Seq[Future[Option[T]]]) = 
    Future.sequence(xs) map (_.flatten)
  
  /**
   * returns finite distribution on Moves given actor name, as future option.  
   */
  def getFutOptFDV(name: String) =
    (getFutOptElems(name)) mapp (
        (vec: Vector[ACElem]) => 
          FiniteDistribution(
            vec map ((elem) => Weighted(elem.moves, elem.weight))
          )
  )
  
  
  /**
   * returns finite distribution on theorems, given actor name, as future option.
   */
  def getFutOptThms(name: String) =
    getFutOptThmElems(name) mapp(
        (vec: Vector[ACThm]) => 
          FiniteDistribution(
            vec map ((elem) => Weighted(elem.pres, elem.weight))
          )
  )
  

//  def getFutOptState(name: String) = 
//    for (optFDM <- getFutOptFDM(name); optFDV <- getFutOptFDV(name)) 
//      yield {(optFDM, optFDV) match {
//        case (Some(fdM), Some(fdV)) => Some((fdM, fdV))
//        case _ => None
//    }
//    }

  /**
   * returns state (fdM, fdV) give name of actor, as future option
   */
  def getFutOptState(name: String) = 
    getFutOptFDM(name) flatMapp (
        (fdM) => 
          getFutOptFDV(name) mapp ((fdV) => (fdM, fdV))
        )
  
  /**
   * returns state (fdM, fdV) given name as actor as future, with default for option,
   * need to specify rank for default (as actor may not exist to look up).      
   */
  def getFutStateDefault(name: String, rank: Int = 2) = {
    val default = (learnerMoves(rank), eVec)
    getFutOptState(name) map (_.getOrElse(default))
  }
  
  def getFutState(name: String) = 
    getFutOptState(name) map (_.get)
  
  /**
   * returns stream of the evolution of weights for a fixed presentation.  
   */
  def thmWeights(thm: Presentation, name: String) = {
    val query = 
      BSONDocument(
        "presentation" -> uwrite(thm),
        "name" -> name) // matches against pickled theorem
    val cursor = 
      thmsDB.
      find(query).
      sort(BSONDocument("loops" -> 1)). // should check if should use 1 or "increasing"
      cursor[ACThm]()
    cursor.collect[Stream]()
  }
}


case class ACMoveWeights(name: String, fdM : FiniteDistribution[AtomicMove], loops: Int)
    