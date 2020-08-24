package provingground.andrewscurtis

import provingground._

import learning._

import reactivemongo.api._
import reactivemongo.bson._

import reactivemongo.api.commands.WriteResult

import reactivemongo.api.indexes._
import reactivemongo.api.bson.collection.BSONSerializationPack

import upickle.default.{write => uwrite, read => uread, _}

import scala.concurrent.ExecutionContext.Implicits.global

import FreeGroups._

import scala.concurrent._

//import Collections._

import MoveGenerator._

object ACMongo extends ACWriter {
  lazy val driver = new MongoDriver()

  lazy val connection = driver.connection(List("localhost"))

  /**
    * The database for andrews-curtis runs.
    */
  implicit lazy val dbFut: Future[DefaultDB] = connection.database("provingground-andrewscurtis")

  /**
    * Collection for andrews-curtis elements - elements have all the info.
    */
  lazy val elemsDBFut = dbFut.map(_("elements"))

  val elemsInd = elemsDBFut.map(_.indexesManager)

  val index = Index(BSONSerializationPack)(
    key = Seq("name" -> IndexType.Ascending, "loops" -> IndexType.Descending),
    name = Some("name_idx"),
    unique = false,
    background = false,
    dropDups = false,
    sparse = false,
    expireAfterSeconds = None,
    storageEngine = None,
    weights = None,
    defaultLanguage = None,
    languageOverride = None,
    textIndexVersion = None,
    sphereIndexVersion = None,
    bits = None,
    min = None,
    max = None,
    bucketSize = None,
    collation = None,
    wildcardProjection = None,
    version = None,
    partialFilter = None,
    options = BSONDocument.empty)

  elemsInd.foreach(_.ensure(index)) // index elements by actor name and loops (descending).

  /**
    * collections of theorems, with total weights.
    */
  lazy val thmsDB = dbFut.map(_("theorems"))

  val thmsInd = thmsDB.map(_.indexesManager)

  thmsInd.foreach(_.ensure(index)) // index theoreme by actor name and loops (descending)

  val thmsPresIndex = 
  Index(BSONSerializationPack)(
    key = Seq("presentation" -> IndexType.Hashed),
    name = Some("name_idx"),
    unique = false,
    background = false,
    dropDups = false,
    sparse = false,
    expireAfterSeconds = None,
    storageEngine = None,
    weights = None,
    defaultLanguage = None,
    languageOverride = None,
    textIndexVersion = None,
    sphereIndexVersion = None,
    bits = None,
    min = None,
    max = None,
    bucketSize = None,
    collation = None,
    wildcardProjection = None,
    version = None,
    partialFilter = None,
    options = BSONDocument.empty)

  thmsInd.foreach(_.ensure(thmsPresIndex)) // also index by hhashed presentation

  /**
    * collection for actor data: names, loops run and anything else stored.
    */
  lazy val actorsDB = dbFut.map(_("actors"))

  /**
    * collection for weights of moves.
    */
  lazy val moveWeightsDB = dbFut.map(_("moveweights"))

  /**
    * implicit writer for AC elements
    */
  implicit object ElemsWriter extends BSONDocumentWriter[ACElem] {
    def write(elem: ACElem) =
      BSONDocument(
        "name"         -> elem.name,
        "moves"        -> uwrite(elem.moves), // pickled form of moves
        "rank"         -> elem.rank,
        "presentation" -> uwrite(elem.pres), // pickled form of presentation
        "loops"        -> elem.loops,
        "weight"       -> elem.weight
      )
  }

  /**
    * implicit reader for andrews-curtis elements
    */
  implicit object ElemsReader extends BSONDocumentReader[ACElem] {
    def read(doc: BSONDocument) = {
      val opt = for (name <- doc.getAs[String]("name");
                     pmoves <- doc.getAs[String]("moves"); // pickled form of  moves
                     rank   <- doc.getAs[Int]("rank");
                     ppres  <- doc.getAs[String]("presentation"); // picked form of presentation
                     loops  <- doc.getAs[Int]("loops");
                     weight <- doc.getAs[Double]("weight"))
        yield
          ACElem(name,
                 uread[Moves](pmoves), //unpickle moves
                 rank,
                 uread[Presentation](ppres), //unpickle presentation
                 weight,
                 loops)
      opt.get
    }
  }

  /**
    * implicit writer for andrews-curtis theorems
    */
  implicit object ThmWriter extends BSONDocumentWriter[ACThm] {
    def write(elem: ACThm) =
      BSONDocument("name"         -> elem.name,
                   "presentation" -> uwrite(elem.pres), // pickled presentation
                   "loops"        -> elem.loops,
                   "weight"       -> elem.weight)
  }

  /**
    * implicit reader for andrews-curtis theorems
    */
  implicit object ThmReader extends BSONDocumentReader[ACThm] {
    def read(doc: BSONDocument) = {
      val opt = for (name <- doc.getAs[String]("name");
                     ppres  <- doc.getAs[String]("presentation"); // pickle presentation
                     loops  <- doc.getAs[Int]("loops");
                     weight <- doc.getAs[Double]("weight"))
        yield
          ACThm(name,
                uread[Presentation](ppres), // unpickle presentation
                weight,
                loops)
      opt.get
    }
  }

  /**
    * implicit writer for moves
    */
  implicit object MoveWeightWriter extends BSONDocumentWriter[ACMoveWeights] {
    def write(elem: ACMoveWeights) =
      BSONDocument("name"  -> elem.name,
                   "fdM"   -> uwrite(elem.fdM),
                   "loops" -> elem.loops)
  }

  /**
    * implicit reader for moves
    */
  implicit object MoveWeightReader extends BSONDocumentReader[ACMoveWeights] {
    def read(doc: BSONDocument) = {
      val opt = for (name <- doc.getAs[String]("name");
                     pfdM  <- doc.getAs[String]("fdM");
                     loops <- doc.getAs[Int]("loops"))
        yield
          ACMoveWeights(name,
                        uread[FiniteDistribution[AtomicMove]](pfdM),
                        loops)
      opt.get
    }
  }

  /**
    * insert element in AC element collection
    */
  def addElem(el: ACElem) =
    elemsDBFut.foreach(_.insert(el))

  /**
    * insert theorem in collection
    */
  def addThm(thm: ACThm) =
    thmsDB.foreach(_.insert(thm))

  /**
    * insert move weights in collection.
    */
  def addMoveWeight(wts: ACMoveWeights) =
    moveWeightsDB.foreach(_.insert(wts))

  /**
    * update number of loops of an actor;
    *   if there is other actor data, this is left unchanged.
    */
  def updateLoops(name: String, loops: Int) = {
    val selector = BSONDocument("name" -> name)

    val init =
      BSONDocument("name" -> name, "loops" -> loops) //initial data if no entry exists

    val modifier =
      BSONDocument("$set" -> BSONDocument("loops" -> loops)) //update

    val checkCursor = actorsDB.map(_.find(selector).cursor[BSONDocument]())

    val emptyFut =
      checkCursor.flatMap(_.headOption map (_.isEmpty)) // check if entry for actor exists.

    emptyFut map
      ((check) =>
        if (check) actorsDB.foreach(_.insert(ordered = false).one(init))
        else actorsDB.foreach(_.update(ordered = false).one(selector, modifier)))
  }

  /**
    * get elements as future given actor name and stage.
    */
  def getFutElemsStep(name: String, loops: Int) = {
    val selector = BSONDocument("name" -> name, "loops" -> loops)
    elemsDBFut.flatMap(_.find(selector).cursor[ACElem]().collect[Vector](-1, Cursor.FailOnError[Vector[ACElem]]()) )
  }

  /**
    * get theorems given actor name and loops as future.
    */
  def getFutThmElemsStep(name: String, loops: Int) = {
    val selector = BSONDocument("name" -> name, "loops" -> loops)
    thmsDB.flatMap(_.find(selector).cursor[ACThm]().collect[Vector](-1, Cursor.FailOnError[Vector[ACThm]]()) )
  }

  /**
    * get (optional, future) weights of moves given actor name and loops.
    */
  def getFutOptFDMStep(name: String, loops: Int) = {
    val selector = BSONDocument("name" -> name, "loops" -> loops)
    moveWeightsDB.flatMap(_.find(selector).cursor[ACMoveWeights]().headOption map
      ((opt) => opt map (_.fdM)))
  }

  def getFutActors() = {
    val entries =
      actorsDB.map(_.find(BSONDocument()).cursor[BSONDocument]().collect[Vector](-1, Cursor.FailOnError[Vector[BSONDocument]]()) )
    entries.flatMap(_.map ((vec) => (vec map (_.getAs[String]("name"))).flatten))
  }

  def getFutStartData() = {
    val entries =
      actorsDB.flatMap(_.find(BSONDocument()).cursor[BSONDocument]().collect[Vector](-1, Cursor.FailOnError[Vector[BSONDocument]]()) )
    entries map
      ((vec) =>
        (vec map
          (_.getAs[String]("start-data") flatMap
            ((d) => uread[List[StartData]](d).headOption))).flatten)
  }

  /**
    * get the number of loops run by an actor as an optional future.
    */
  def getFutOptLoops(name: String) = {
    val selector = BSONDocument("name" -> name)
    val futOpt   = actorsDB.flatMap(_.find(selector).cursor[BSONDocument]().headOption)
    futOpt map ((opt) => (opt) flatMap ((doc) => doc.getAs[Int]("loops")))
  }

  /**
    * get elements given only actor name (as future, option)
    */
  def getFutOptElems(name: String) =
    (getFutOptLoops(name) flatMap
      ({
        case Some(loops) =>
          getFutElemsStep(name, loops - 1) map ((vec) => Some(vec))
        case None => Future.successful(None)
      }))

  /**
    * get theorems given only actor name (as future, option)
    */
  def getFutOptThmElems(name: String) =
    getFutOptLoops(name) flatMap
      ({
        case Some(loops) =>
          getFutThmElemsStep(name, loops - 1) map ((vec) => Some(vec))
        case None => Future.successful(None)
      })

  /**
    * get weights of atomic moves given actor name (as future, option)
    */
  def getFutOptFDM(name: String) =
    getFutOptLoops(name) flatMap
      ({
        case Some(loops) => getFutOptFDMStep(name, loops - 1)
        case None        => Future.successful(None)
      })

  /**
    * mapping future options,
    * should upgrade to monadic implicit class.
    */
  def mapFutOpt[S, T](futOpt: Future[Option[S]])(fn: S => T) = {
    futOpt map ((opt) => opt map (fn))
  }

  /**
    * monadic operations for future options
    */
  implicit class FutOptMonad[A](futOpt: Future[Option[A]]) {
    def mapp[B](fn: A => B) = futOpt map ((opt) => opt map (fn))

    def flatMapp[B](fn: A => Future[Option[B]]) = {
      futOpt flatMap {
        case Some(b) => fn(b)
        case None    => Future.successful(None)
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
    (getFutOptElems(name)) mapp
      ((vec: Vector[ACElem]) =>
        FiniteDistribution(
          vec map ((elem) => Weighted(elem.moves, elem.weight))))

  /**
    * returns finite distribution on theorems, given actor name, as future option.
    */
  def getFutOptThms(name: String) =
    getFutOptThmElems(name) mapp
      ((vec: Vector[ACThm]) =>
        FiniteDistribution(
          vec map ((elem) => Weighted(elem.pres, elem.weight))))

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
    getFutOptFDM(name) flatMapp
      ((fdM) => getFutOptFDV(name) mapp ((fdV) => (fdM, fdV)))

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
      BSONDocument("presentation" -> uwrite(thm), "name" -> name) // matches against pickled theorem
    val cursor = thmsDB.map(_
      .find(query)
      .sort(BSONDocument("loops" -> 1))
      . // should check if should use 1 or "increasing"
      cursor[ACThm]())
    cursor.flatMap(_.collect[LazyList](-1, Cursor.FailOnError[LazyList[ACThm]]()))
  }

  def allThmWeights(name: String) = {
    val query  = BSONDocument("name" -> name) // matches against pickled theorem
    val cursor = thmsDB.map(_.find(query).cursor[ACThm]())
    cursor.flatMap(_.collect[Vector](-1, Cursor.FailOnError[Vector[ACThm]]()))
  }

  def thmSupp(name: String) = {
    val query  = BSONDocument("name" -> name) // matches against pickled theorem
    val cursor = thmsDB.map(_.find(query).cursor[ACThm]())
    cursor.flatMap(_.collect[Set](-1, Cursor.FailOnError[Set[ACThm]]()) map ((fut) => fut map (_.pres)) map (_.toVector))
  }

  def thmView(
      thms: Vector[ACThm])(thm: Presentation, name: String, loops: Int) =
    ((thm.toString) +: (ACThm.weightVector(thms, loops)(thm) map (_.toString)))
      .mkString(",")

  def thmSaveCSV(thms: Vector[ACThm])(name: String,
                                      loops: Int,
                                      dir: String = "ac-data") = {
    import ammonite.ops._
    val wd   = pwd / "data" / dir
    val file = wd / s"$name-thms.csv"
    def supp = (thms map (_.pres)).toSet.toVector
    rm(file)
    supp.foreach((thm) =>
      write.append(file, thmView(thms)(thm, name, loops) + "\n"))
  }

  def thmsCSV(name: String, dir: String = "ac-data") = {
    for (thms <- allThmWeights(name); optLoops <- getFutOptLoops(name))
      yield optLoops.foreach((loops) => thmSaveCSV(thms)(name, loops, dir))
  }
}

case class ACMoveWeights(name: String,
                         fdM: FiniteDistribution[AtomicMove],
                         loops: Int)
