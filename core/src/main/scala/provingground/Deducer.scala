package provingground
import provingground.{FiniteDistribution => FD, TruncatedDistribution => TD, ProbabilityDistribution => PD, TermLang => TL}

import HoTT._

import upickle.default._

//import scala.language.postfixOps

import scala.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Generating terms from given ones using the main HoTT operations, and the adjoint of this generation.
  * This is viewed as deduction.
  * Generation is a map on probability distributions,
  * but the adjoint regards truncated distributions as the tangent space, and restricts domain to finite distributions.
  *
  */
object Deducer {
  type InvMap = Vector[(Term, Set[(Term, Term)])]

  /**
    * generating optionally using function application, with function and argument generated recursively;
    * to be mixed in using `<+?>`
    */
  def appln(rec: => (PD[Term] => PD[Term]))(p: PD[Term]) =
    rec(p) flatMap ((f) =>
          if (isFunc(f))
            rec(p) map (Unify.appln(f, _))
          else
            FD.unif(None: Option[Term]))

  def memAppln(rec: => (PD[Term] => PD[Term]))(
      p: PD[Term])(save: (Term, Term, Term) => Unit) = {
    rec(p) flatMap ((f) =>
          if (isFunc(f))
            rec(p) map ((x) =>
                  Unify.appln(f, x) map ((y) => { save(f, x, y); y }))
          else
            FD.unif(None: Option[Term]))
  }

  def eqSubs(rec: => (PD[Term] => PD[Term]))(
      p: PD[Term])(save: (Term, IdentityTyp[Term], Term) => Unit) = {
    rec(p) flatMap {
      case eq @ IdentityTyp(dom, lhs: Term, rhs: Term) =>
        rec(p) map ((x) =>
              if (x.typ == dom)
                Some(x.subs(lhs, rhs)) map ((y) => {
                      save(x, eq.asInstanceOf[IdentityTyp[Term]], y); y
                    })
              else None)
      case _ =>
        FD.unif(None: Option[Term])
    }
  }

  /**
    * generating optionally as lambdas, with function and argument generated recursively;
    * to be mixed in using `<+?>`
    */
  def lambda(varweight: Double)(
      rec: => (PD[Term] => PD[Term]))(p: PD[Term]): PD[Option[Term]] =
    rec(p) flatMap ({
      case tp: Typ[u] =>
        val x = tp.Var
        val newp = p <+> (FD.unif(x), varweight)
        (rec(newp)) map ((y: Term) => TL.lambda(x, y))
      case _ => FD.unif(None)
    })

  /**
    * generating optionally as pi's, with function and argument generated recursively;
    * to be mixed in using `<+?>`
    */
  def pi(varweight: Double)(
      rec: => (PD[Term] => PD[Term]))(p: PD[Term]): PD[Option[Term]] =
    rec(p) flatMap ({
      case tp: Typ[u] =>
        val x = tp.Var
        val newp = p <+> (FD.unif(x), varweight)
        (rec(newp)) map ((y: Term) => TL.pi(x, y))
      case _ => FD.unif(None)
    })

  /**
    * given a type, returns optionally values of lambda terms with variable of the given type
    * with variable in values from the above `variable` object
    */
  def lambdaValue[U <: Term with Subs[U]](variable: U): Term => Option[Term] = {
    case l: LambdaLike[u, v] if l.variable.typ == variable.typ =>
      Some(l.value replace (l.variable, variable))
    case _ => None
  }

  def piValue[U <: Term with Subs[U]](variable: U): Term => Option[Term] = {
    case pt: PiTyp[u, v] if pt.fibers.dom == variable.typ =>
      val x = variable.asInstanceOf[u]
      val codom = pt.fibers(x)
      Some(codom)
    case FuncTyp(dom: Typ[u], codom: Typ[v]) if dom == variable.typ =>
      Some(codom)
    case _ => None
  }

  def feedback(absTheorems: FD[Typ[Term]],
               absThmsByProofs: FD[Typ[Term]],
               proofs: Map[Typ[Term], FD[Term]],
               vars: Vector[Weighted[Term]],
               lambdaWeight: Double,
               piWeight: Double) = {
    import TermBucket.{mkPi, mkLambda}
    import math.log
    def forTyp(typ: Typ[Term]) = {
      val weightedTerms = proofs(typ).pmf
      val absTyp = mkPi(vars, piWeight)(Weighted(typ, 1)).elem
      val entDiff = -log(absThmsByProofs(absTyp)) + log(absTheorems(absTyp))
      def termWeight(wt: Weighted[Term]) =
        entDiff * mkLambda(vars, lambdaWeight)(wt).weight / absThmsByProofs(
            absTyp)
      val typPMF =
        weightedTerms map ((wt) => Weighted(wt.elem, termWeight(wt)))
      FD(typPMF)
    }
    (proofs.keys map (forTyp)).fold(FD.empty[Term])(_ ++ _)
  }

  def unpickle(str: String) = read[PickledTermPopulation](str).unpickle

  import Unify._

  def unifInv[U <: Term with Subs[U]](
      term: Term, invMap: Vector[(Term, Set[(U, Term)])]) = {
    val optInverses =
      invMap flatMap {
        case (result, fxs) =>
          {
            val uniMapOpt = unify(result, term, isVar)
            val newInvOpt =
              uniMapOpt map { (uniMap) =>
                fxs map {
                  case (f, x) => (multisub(f, uniMap), multisub(x, uniMap))
                }
              }
            newInvOpt
          }
      }
    optInverses.flatten.toSet filter ((fx) =>
          Unify.appln(fx._1, fx._2) == Some(term))
  }

  def hashedUnifInv[U <: Term with Subs[U]](
      term: Term,
      hashedInvMap: Map[ShapeTree, Vector[(Term, Set[(U, Term)])]]) = {
    val invMapSet =
      (TermShapeTree(term).subTrees map ((shape) =>
                hashedInvMap.getOrElse(shape, Vector())))
    val invMap = invMapSet.fold(Vector())(_ ++ _)
    unifInv(term, invMap)
  }

  class HashedUnifInv[U <: Term with Subs[U]](
      invMap: Vector[(Term, Set[(U, Term)])])
      extends (Term => Set[(U, Term)]) {
    val hashedInvMap = invMap groupBy ((kv) => TermShapeTree(kv._1))

    def apply(term: Term) = hashedUnifInv(term, hashedInvMap)
  }

  def flow(fd: FD[Term], shifts: FD[Term]) = {
    val newpmf = for (Weighted(x, p) <- fd.pmf) yield
      Weighted(x, p * math.exp(shifts(x)))
    FD(newpmf).flatten.normalized()
  }
}

class BasicDeducer(applnWeight: Double = 0.2,
                   lambdaWeight: Double = 0.2,
                   piWeight: Double = 0.2,
                   varWeight: Double = 0.3){
  import Deducer._

  def func(pd: PD[Term]): PD[Term] =
    pd.<+?>(appln(func)(pd), applnWeight)
      .<+?>(lambda(varWeight)(func)(pd), lambdaWeight)
      .<+?>(pi(varWeight)(func)(pd), lambdaWeight)

                   }

case class Deducer(applnWeight: Double = 0.2,
                   lambdaWeight: Double = 0.2,
                   piWeight: Double = 0.2,
                   varWeight: Double = 0.3,
                   vars: Vector[Weighted[Term]] = Vector(),
                   propDecay: Double = 0.5,
                   cutoff: Double = 0.01,
                   feedbackScale: Double = 0.1,
                   abstractionWeight: Double = 0.3,
                   genMemory: Double = 0.8) {
  import Deducer._

  import HoTT.isVar

//  import Unify.{unify, multisub}

  object bucket extends TermBucket

  object absBucket extends WeightedTermBucket {}

  type Prob = Term => Double

  def shifted(fd: FD[Term], td: TD[Term]) = {
    val shifts = td.getFD(cutoff) getOrElse (FD.empty[Term])
    val newpmf = for (Weighted(x, p) <- fd.pmf) yield
      Weighted(x, p * math.exp(shifts(x)))
    FD(newpmf)
  }

  /**
    * given a truncated distribution of terms and a type,
    * returns the truncated distribution of `value`s of lambda terms of that type;
    * with variable in the values from the above `variable` object
    */
  // def lambdaTD(td: TD[Term])(variable: Term) =
  //   (td mapOpt (lambdaValue(variable))) //<+> (TD.atom(variable) <*> varWeight)

  def lambdaFD(fd: Prob)(variable: Term) =
    (y: Term) => fd(HoTT.lambda(variable)(y))
  //  (fd mapOpt (lambdaValue(variable))) ++ (FD.unif(variable) * varWeight)

  def lambdaProb(prob: Prob)(variable: Term): Prob =
    (y: Term) => prob(HoTT.lambda(variable)(y))

  // def piTD(td: TD[Term])(variable: Term) =
  //   (td mapOpt (piValue(variable))) //<+> (TD.atom(variable) <*> varWeight)

  def piFD(fd: Prob)(variable: Term): Prob = {
    case tp: Typ[u] => fd(HoTT.pi(variable)(tp))
    case _ => 0
  }
//    (fd mapOpt (piValue(variable))) ++ (FD.unif(variable) * varWeight)

  // def func(pd: PD[Term]): PD[Term] =
  //   pd.<+?>(appln(func)(pd), applnWeight)
  //     .<+?>(lambda(varWeight)(func)(pd), lambdaWeight)
  //     .<+?>(pi(varWeight)(func)(pd), lambdaWeight)
  //
  // val invImageMap: scala.collection.mutable.Map[Term, Set[(Term, Term)]] =
  //   scala.collection.mutable.Map()
  //
  // def invImage(accum: InvMap = Vector()) =
  //   Unify.purgedInvVector(invImageMap.toVector, accum, isVar)
  //
  // var cumInvImage: InvMap = Vector()
  //
  // def getInvImage() = {
  //   val combined = invImage(cumInvImage)
  //   cumInvImage = combined
  //   invImageMap.clear()
  //   (t: Term) =>
  //     unifInv(t, combined)
  // }
  //
  // def getHashedInvImage(): Term => Set[(Term, Term)] = {
  //   val combined = invImage(cumInvImage)
  //   cumInvImage = combined
  //   invImageMap.clear()
  //   new HashedUnifInv(cumInvImage)
  // }
  //
  // def applnInvImage(term: Term) = unifInv(term, invImageMap.toVector)
  //
  // val subsInvMap: scala.collection.mutable.Map[
  //     Term, Set[(IdentityTyp[Term], Term)]] = scala.collection.mutable.Map()
  //
  // def subsInvImage(term: Term) = unifInv(term, subsInvMap.toVector)
  //
  // def subsInvImages = TermToExpr.rebuildMap(subsInvMap.toMap)
  //
  // def save(f: Term, x: Term, y: Term) =
  //   invImageMap(y) = invImageMap.getOrElse(y, Set()) + ((f, x))
  //
  // def saveSubs(x: Term, eq: IdentityTyp[Term], result: Term) =
  //   subsInvMap(result) = subsInvMap.getOrElse(result, Set()) + ((eq, x))
  //
  // def memFunc(pd: PD[Term]): PD[Term] =
  //   pd.<+?>(memAppln(memFunc)(pd)(save), applnWeight)
  //     .<+?>(lambda(varWeight)(memFunc)(pd), lambdaWeight)
  //     .<+?>(pi(varWeight)(memFunc)(pd), lambdaWeight)
  //
  // def sample(pd: PD[Term], n: Int) =
  //   (1 to n).foreach((_) => bucket.append(memFunc(pd).next))
  //
  class Sampler {
    object bucket extends TermBucket

    val invImageMap: scala.collection.mutable.Map[Term, Set[(Term, Term)]] =
      scala.collection.mutable.Map()

    def save(f: Term, x: Term, y: Term) =
      invImageMap(y) = invImageMap.getOrElse(y, Set()) + ((f, x))

    def memFunc(pd: PD[Term]): PD[Term] =
      pd.<+?>(memAppln(memFunc)(pd)(save), applnWeight)
        .<+?>(lambda(varWeight)(memFunc)(pd), lambdaWeight)
        .<+?>(pi(varWeight)(memFunc)(pd), lambdaWeight)

    def mkSample(pd: PD[Term], n: Int) =
      (1 to n).foreach((_) => bucket.append(memFunc(pd).next))

    def invImage(accum: InvMap = Vector()) =
      Unify.purgedInvVector(invImageMap.toVector, accum, isVar) map {
        case (x, s) => (x, Unify.purgedPairs(s))
      }

    def getPopulation(accum: InvMap = Vector()) =
      TermPopulation(bucket.getTermDistMap,
                     bucket.getTypDist,
                     bucket.getThmsByProofs,
                     vars,
                     lambdaWeight,
                     piWeight,
                     invImage(accum))
  }

  def getSample(pd: PD[Term], n: Int, accum: InvMap = Vector()) = {
    val sampler = new Sampler
    sampler.mkSample(pd, n)
    sampler.getPopulation(accum)
  }

  import TermBucket.{lambdaDist, piDist}

  def absFD(fd: FD[Term]) = {
    val tfd = fd filter (isTyp) map { case tp: Typ[u] => tp }
    (fd ++
        ((lambdaDist(vars, lambdaWeight)(fd) ++
                piDist(vars, piWeight)(tfd).map((t) => t: Term)) * abstractionWeight))
      .normalized()
  }

  def shiftFD(popln: TermPopulation, cumApplnInv: InvMap = Vector()) = {
    val td = TD.PosFD(popln.feedback * feedbackScale)
    val back =
      backProp(propDecay, popln.applnInv)((t: Term) => popln.terms(t))(td)
    back.getFD(cutoff).getOrElse(FD.empty[Term])
  }

  def nextDistribution(fd: FD[Term],
                       n: Int,
                       memory: Boolean = true,
                       accum: InvMap = Vector(),
                       smooth: FD[Term] => FD[Term] = identity) = {
    import Deducer.flow

    val pop = getSample(fd, n, accum)

    val feed = pop.feedback

    val shift = shiftFD(pop)

    val newPop = absFD(pop.terms)

    val mixedPop =
      if (memory)
        ((fd * genMemory ++ (newPop * (1 - genMemory)))).flatten
      else newPop.flatten

    (smooth(flow(mixedPop, shift)).flatten, pop.applnInvMap)
  }

  def learnerNextDistribution(fd: FD[Term],
                              theorems: FD[Typ[Term]],
                              n: Int,
                              memory: Boolean = true,
                              accum: InvMap = Vector(),
                              smooth: FD[Term] => FD[Term] = identity) = {
    val absTheorems = piDist(vars, piWeight)(theorems)

    import Deducer.flow

    val pop = getSample(fd, n, accum)

    val feed = pop.learnerFeedback(absTheorems)

    val shift = shiftFD(pop)

    (smooth(flow(fd, shift)), pop.applnInvMap)
  }

  class BufferedRun(initDist: FD[Term],
                    initBatch: Int,
                    batchSize: Int,
                    halt: => (BufferedRun => Boolean) = (bfr) =>
                      bfr.getElapsedTime > (1000 * 5 * 60),
                    save: FD[Term] => Unit = (_) => (),
                    smooth: FD[Term] => FD[Term] = identity) { self =>

    import scala.collection.mutable.ArrayBuffer
    private val distBuffer: ArrayBuffer[FD[Term]] = ArrayBuffer()

    val theorems =
      (initDist filter (isTyp) map { case tp: Typ[u] => tp }).flatten
        .normalized()

    def saveMem(fd: FD[Term]) = {
      distBuffer.append(fd);
      eventHook(fd)
      save(fd)
    }

    def getTimeSeries = distBuffer.toVector

    private var runHook: Boolean = true

    var eventHook: FD[Term] => Unit = (_) => ()

    def onChange(react: FD[Term] => Unit) = {
      val prevHook = eventHook
      eventHook = (fd: FD[Term]) => { react(fd); prevHook(fd) }
    }

    def stop() = { runHook = false }

    val startTime: Long = System.currentTimeMillis()

    def getElapsedTime = System.currentTimeMillis() - startTime

    private var loops: Int = 0

    def getLoops = loops

    def run = Future(await)

    def learn = Future(learnAwait)

    def iteratorPair = {
      val start = nextDistribution(
          initDist, initBatch, false, Vector(), smooth)
      def func(pair: (FD[Term], InvMap)): (FD[Term], InvMap) =
        nextDistribution(pair._1, batchSize, true, pair._2, smooth)

      Iterator.iterate(start)(func)
    }

    def iterator = iteratorPair map (_._1)

    def iteratorWhile(p: FD[Term] => Boolean = (_) => runHook && !halt(self)) =
      iterator.takeWhile(p)

    def iterate(s: FD[Term] => Unit = saveMem) = Future(iterator.foreach(s))

    def iterateWhile(p: FD[Term] => Boolean = (_) => runHook && !halt(self))(
        s: FD[Term] => Unit = saveMem) =
      Future(iteratorWhile(p).foreach(s))

    def awaitIterate = iteratorWhile().foreach(saveMem)

    def await = {
      var mutDistAccum = nextDistribution(
          initDist, initBatch, false, Vector(), smooth)
      saveMem(mutDistAccum._1)
      while (runHook && !halt(self)) {
        loops += 1
        println(s"Time : $getElapsedTime; Loops: $getLoops")
        val (dist, accum) = nextDistribution(
            mutDistAccum._1, batchSize, true, mutDistAccum._2, smooth)
        saveMem(mutDistAccum._1)
        mutDistAccum = (dist, accum)
      }
      println(s"Halted: (_, $initBatch, $batchSize)")
      getTimeSeries
    }

    def learnAwait = {
      var mutDistAccum = learnerNextDistribution(
          initDist, theorems, initBatch, false, Vector(), smooth)
      saveMem(mutDistAccum._1)
      while (runHook && !halt(self)) {
        loops += 1
        println(s"Time : $getElapsedTime; Loops: $getLoops")
        val (dist, accum) = learnerNextDistribution(mutDistAccum._1,
                                                    theorems,
                                                    batchSize,
                                                    true,
                                                    mutDistAccum._2,
                                                    smooth)
        saveMem(mutDistAccum._1)
        mutDistAccum = (dist, accum)
      }
      println(s"Halted: (_, $initBatch, $batchSize)")
      getTimeSeries
    }
  }

  def funcUniPropTerm(backProp: => (Prob => TD[Term] => TD[Term]))(
      fd: Prob, invImage: Term => Set[(Term, Term)]): Term => TD[Term] =
    (result) => {
      val tds =
        invImage(result).toVector map {
          case (f, x) =>
            val scale = applnWeight * fd(f) * fd(x) / fd(result)
            backProp(fd)(TD.FD(FD.unif(f, x)) <*> scale)
        }
      TD.bigSum(tds)
    }

  def funcUniProp(backProp: => (Prob => TD[Term] => TD[Term]))(
      fd: Prob, invImage: Term => Set[(Term, Term)])(td: TD[Term]) =
    td flatMap (funcUniPropTerm(backProp)(fd, invImage))

  // def eqSubsPropTerm(backProp: => (Prob => TD[Term] => TD[Term]))(
  //     fd: Prob): Term => TD[Term] =
  //   (result) =>
  //     subsInvMap.get(result) map (
  //         (v) => {
  //           val tds =
  //             v.toVector map {
  //               case (eq, x) =>
  //                 val eqSubsWeight: Double = 0.0
  //                 val scale = eqSubsWeight * fd(eq) * fd(x) / fd(result)
  //                 backProp(fd)(TD.FD(FD.unif(eq, x)) <*> scale)
  //             }
  //           TD.bigSum(tds)
  //         }
  //     ) getOrElse (TD.Empty[Term])
  //
  // def eqSubsProp(backProp: => (Prob => TD[Term] => TD[Term]))(fd: Prob)(
  //     td: TD[Term]) =
  //   td flatMap (eqSubsPropTerm(backProp)(fd))

  def lambdaPropVarTerm(backProp: => (Prob => TD[Term] => TD[Term]))(
      fd: Prob): Term => TD[Term] = {
    case l: LambdaLike[_, _] =>
      val x = l.variable
      val scale = lambdaWeight * fd(x.typ) * (lambdaFD(fd)(x)(l.value)) / fd(l)
      val atom = TD.atom(l.variable.typ: Term) <*> scale
      backProp(fd)(atom)
    case _ => TD.Empty[Term]
  }

  def lambdaPropVar(backProp: => (Prob => TD[Term] => TD[Term]))(fd: Prob)(
      td: TD[Term]) =
    td flatMap (lambdaPropVarTerm(backProp)(fd))

  def lambdaPropValuesTerm(backProp: => (Prob => TD[Term] => TD[Term]))(
      fd: Prob): Term => TD[Term] = {
    case l: LambdaLike[u, v] =>
      val x = l.variable
      val y = l.value
      val lfd = lambdaFD(fd)(x)
      val scale = lambdaWeight * fd(x.typ) * lfd(y) / fd(l)
      val atom = TD.atom(y: Term) <*> scale
      (backProp(lfd)(atom)) filter ((z) => !(z.dependsOn(x)))
    case _ => TD.Empty[Term]
  }

  def lambdaPropValues(backProp: => (Prob => TD[Term] => TD[Term]))(fd: Prob)(
      td: TD[Term]) =
    td flatMap (lambdaPropValuesTerm(backProp)(fd))

  def piPropVarTerm(backProp: => (Prob => TD[Term] => TD[Term]))(
      fd: Prob): Term => TD[Term] = {
    case pt: PiTyp[u, v] =>
      val x = pt.fibers.dom.Var
      val codom = pt.fibers(x)
      val scale = piWeight * fd(pt.fibers.dom) * piFD(fd)(x)(codom) / fd(pt)
      val atom = TD.atom(pt.fibers.dom: Term) <*> scale
      backProp(fd)(atom)
    case ft @ FuncTyp(dom: Typ[u], codom: Typ[v]) =>
      val atom = TD.atom(dom: Term)
      val x = dom.Var
      val scale = piWeight * fd(dom) * piFD(fd)(x)(codom) / fd(ft)
      backProp(fd)(atom)
    case _ => TD.Empty[Term]
  }

  def piPropVar(backProp: => (Prob => TD[Term] => TD[Term]))(fd: Prob)(
      td: TD[Term]) =
    td flatMap (piPropVarTerm(backProp)(fd))

  def piPropValuesTerm(backProp: => (Prob => TD[Term] => TD[Term]))(
      fd: Prob): Term => TD[Term] = {
    case pt: PiTyp[u, v] =>
      val x = pt.fibers.dom.Var
      val codom = pt.fibers(x)
      val pfd = piFD(fd)(x)
      val scale = piWeight * fd(pt.fibers.dom) * pfd(codom) / fd(pt)
      val atom = TD.atom(codom: Term) <*> scale
      backProp(pfd)(atom) filter ((z) => !(z.dependsOn(x)))
    case ft @ FuncTyp(dom: Typ[u], codom: Typ[v]) =>
      val atom = TD.atom(codom: Term)
      val x = dom.Var
      val pfd = piFD(fd)(x)
      val scale = piWeight * fd(dom) * pfd(codom) / fd(ft)
      backProp(pfd)(atom)
    case _ => TD.Empty[Term]
  }

  def piPropValues(backProp: => (Prob => TD[Term] => TD[Term]))(fd: Prob)(
      td: TD[Term]) =
    td flatMap (piPropValuesTerm(backProp)(fd))

  def backProp(epsilon: Double, invImage: Term => Set[(Term, Term)])(
      fd: Prob): TD[Term] => TD[Term] =
    (td) =>
      td <*> (1 - epsilon) <+>
      (funcUniProp(backProp(epsilon, invImage))(fd, invImage)(td) <*> epsilon) <+>
      (lambdaPropVar(backProp(epsilon, invImage))(fd)(td) <*> epsilon) <+>
      (lambdaPropValues(backProp(epsilon, invImage))(fd)(td) <*> epsilon) <+>
      (piPropVar(backProp(epsilon, invImage))(fd)(td) <*> epsilon) <+>
      (piPropValues(backProp(epsilon, invImage))(fd)(td) <*> epsilon)

  def getAbstractTheorems =
    piDist(vars, piWeight)(bucket.getTheorems)

  def getAbstractTheoremsByProofs =
    piDist(vars, piWeight)(bucket.getThmsByProofs)

  def abstractTyps(typ: Typ[Term]) =
    (TermBucket.mkPi(vars, 1)(Weighted(typ, 1))).elem

  def getElapsedTime = bucket.elapsedTime

  def getLoops = bucket.loops

  /**
    * proofs of an abstracted theorem
    */
  def getProofs(absTyp: Typ[Term]) = {
    val typs = bucket.getThmsByProofs
//    val vars = vars map ((t) => Weighted(t, 1))
    val origTyps =
      typs.supp filter ((tp) =>
            (TermBucket.mkPi(vars, 1)(Weighted(tp, 1))).elem == absTyp)
    val termMap = bucket.getTermDistMap
    val vec: Vector[FD[Term]] =
      origTyps map ((tp) => termMap.getOrElse(tp, FD.empty[Term]))
    vec.fold(FD.empty[Term])(_ ++ _)
  }

  def getFeedback = {
    val thmtyps = bucket.getThmsByProofs
    feedback(getAbstractTheorems,
             getAbstractTheoremsByProofs,
             bucket.getTermDistMap,
             vars,
             lambdaWeight,
             piWeight)
  }
}

import Deducer._

case class TermPopulation(termsByType: Map[Typ[Term], FD[Term]],
                          types: FD[Typ[Term]],
                          thmsByProofs: FD[Typ[Term]],
                          vars: Vector[Weighted[Term]],
                          lambdaWeight: Double,
                          piWeight: Double,
                          applnInvMap: InvMap = Vector()) { self =>
  val theorems =
    FD(thmsByProofs.supp map ((t) => Weighted(t, types(t)))).normalized()

  import TermBucket._

  import Deducer._

  val applnInv = (t: Term) => unifInv(t, applnInvMap)

  def ++(that: TermPopulation) = {
    val termsByType =
      ((this.termsByType.keySet union that.termsByType.keySet) map (
              (typ: Typ[Term]) =>
                (typ,
                 this.termsByType.getOrElse(typ, FD.empty[Term]) ++ that.termsByType
                   .getOrElse(typ, FD.empty[Term]))
          )).toMap
    TermPopulation(termsByType,
                   this.types ++ that.types,
                   this.thmsByProofs ++ that.thmsByProofs,
                   vars,
                   lambdaWeight,
                   piWeight,
                   applnInvMap ++ that.applnInvMap)
  }

  def fromFD(fd: FD[Term]) = {
    val termsByType =
      (fd.pmf groupBy (_.elem.typ: Typ[Term])) mapValues (FD(_))
    val types = (fd mapOpt {
          case tp: Typ[u] => Some(tp: Typ[Term])
          case _ => None
        }).normalized()
    val thmsByProofs = fd map (_.typ)
    TermPopulation(termsByType,
                   types,
                   thmsByProofs,
                   vars,
                   lambdaWeight,
                   piWeight,
                   applnInvMap)
  }

  def +++(that: FiniteDistribution[Term]) =
    ++(fromFD(that))

  def *(scale: Double) =
    TermPopulation(termsByType mapValues ((fd) => fd * scale),
                   types * scale,
                   thmsByProofs * scale,
                   vars,
                   lambdaWeight,
                   piWeight,
                   applnInvMap)

  def normalized =
    TermPopulation(termsByType mapValues ((fd) => fd.normalized()),
                   types.normalized(),
                   thmsByProofs.normalized(),
                   vars,
                   lambdaWeight,
                   piWeight,
                   applnInvMap)

  val terms = termsByType.values.fold(FD.empty[Term])(_ ++ _)

  val abstractTheorems = piDist(vars, piWeight)(theorems)

  val abstractTheoremsByProofs = piDist(vars, piWeight)(thmsByProofs)

  lazy val feedback = Deducer.feedback(abstractTheorems,
                                       abstractTheoremsByProofs,
                                       termsByType,
                                       vars,
                                       lambdaWeight,
                                       piWeight)

  def learnerFeedback(absTheorems: FD[Typ[Term]]) =
    Deducer.feedback(absTheorems,
                     abstractTheoremsByProofs,
                     termsByType,
                     vars,
                     lambdaWeight,
                     piWeight)

  def pickledPopulation = {
    import FreeExpr.writeTerm
    val termsByType = for ((typ, terms) <- this.termsByType) yield
      (writeTerm(typ),
       (terms map (writeTerm)).pmf map (PickledWeighted.pickle))

    PickledTermPopulation(
        termsByType,
        (types map (writeTerm)).pmf map (PickledWeighted.pickle),
        (thmsByProofs map (writeTerm)).pmf map (PickledWeighted.pickle),
        vars map { case Weighted(t, w) => PickledWeighted(writeTerm(t), w) },
        lambdaWeight,
        piWeight
    )
  }

  def pickle = write[PickledTermPopulation](pickledPopulation)
}

case class PickledTermPopulation(
    termsByType: Map[String, Vector[PickledWeighted]],
    types: Vector[PickledWeighted],
    thmsByProofs: Vector[PickledWeighted],
    vars: Vector[PickledWeighted],
    lambdaWeight: Double,
    piWeight: Double) {
  import FreeExpr._
  def unpickle = {
    val termsByType = for ((typ, termsPMF) <- this.termsByType) yield
      (readTyp(typ), FD(termsPMF map ((pw) => pw.map(readTerm))))
    TermPopulation(
        termsByType,
        FD(types map ((pw) => pw map (readTyp))),
        FD(types map ((pw) => pw map (readTyp))),
        vars map ((pw) => pw.map(readTerm)),
        lambdaWeight,
        piWeight
    )
  }
}
