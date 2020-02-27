package provingground.learning
import provingground._, HoTT._
import provingground.induction.ExstInducDefn
import provingground.learning.HoTTMessages.Proved
import provingground.learning.HoTTMessages.Contradicted

/**
  * Messages to be posted for autonomous/interactive running.
  * These can be posted by a user, rule-based bot or a deep-learning system;
  * from the point of view of deep learning, these are moves.
  *
  * Some messages are instructions for what to do next; some report results
  * and some provide data for use in the future.
  *
  *
  * Some types like `LocalProver`, `ExpressionEval`, `TermGenParams` etc can be posted in their raw form.
  */
object HoTTMessages {

  /**
    * goals to seek, through whatever means
    *
    * @param goals the goals
    */
  case class SeekGoals(goals: FiniteDistribution[Typ[Term]])

  /**
    * A goal to seek, often derived from others; if the others are solved or negated, we stop seeking
    *
    * @param goal the goal
    * @param forConsequences the consequences for which we seek this, if any; if empty these have no effect
    */
  case class SeekGoal(goal: Typ[Term], forConsequences: Set[Typ[Term]] = Set())

  /**
    * an initial term state from which to evolve, perhaps just to generate types
    *
    * @param ts the term-state
    */
  case class InitState(
      ts: TermState,
      weight: Double
  )

  /**
    * terms to use for proving, possibly the axioms etc not used in generating types.
    *
    * @param terms the term distribution to use
    */
  case class ConsiderTerms(terms: FiniteDistribution[Term])

  /**
    * introduce inductive types
    *
    * @param inducs distribution on inductive types introduced
    */
  case class ConsiderInductiveTypes(inducs: FiniteDistribution[ExstInducDefn])

  /**
    * modifying parameters, for instance excluding islands, strengthening backward reasoning etc
    *
    * @param modification the modifications
    */
  case class ModifyParams(modification: TermGenParams => TermGenParams)

  /**
    * result of evolution, say of a local-prover; could be just from equations
    *
    * @param ts the evolved state
    */
  case class FinalState(ts: TermState)

  /**
    * Result of generation and normalization
    *
    * @param eqn resulting equation nodes to use
    */
  case class GeneratedEquationNodes(eqn: Set[EquationNode])

  /**
    * instruction to seek lemmas
    *
    * @param weightPower power to flatten distributions
    * @param scale scale?
    */
  case class SeekLemmas(weightPower: Double, scale: Double)

  /**
    * lemmas that have been identified based on non-triviality (and possibly goals) from a final state.
    *
    * @param lemmas the lemmas with weights
    *
    */
  case class Lemmas(
      lemmas: Vector[(Typ[Term], Double)]
  )

  /**
    * instruction to use a lemma - could be as a tangent or mixing in to generators
    * if mixing in just call `ConsiderTerms`
    *
    * @param lemma lemma statement
    * @param proof proof term
    * @param weight weight
    */
  case class UseLemma(lemma: Typ[Term], weight: Double)

  /**
    * proceed by tangent evolution, perhaps from a lemma
    *
    * @param term tangent direction
    * @param weight weight
    */
  case class TangentWithTerm(term: Term, weight: Double)

  /**
    * result of goal chomping
    *
    * @param successes results proved
    * @param failures results neither proved nor disproved
    * @param eqns equations contributed as a by-product
    */
  case class ChompResult(
      successes: Vector[StrategicProvers.Successes],
      failures: Vector[Typ[Term]],
      eqns: Set[EquationNode]
  )

  /**
    * instruction to generate subgoals from a given goal, using a function with eventual codomain, induction etc
    *
    */
  trait ReasonBackward {
    val goal: Typ[Term]
  }

  case class SeekInduction(goal: Typ[Term]) extends ReasonBackward

  case class SeekEventualCodomain(goal: Typ[Term]) extends ReasonBackward

  case class Skolemize(goal: Typ[Term]) extends ReasonBackward

  /**
    * backward reasoning for special types - products, co-products, Sigma-types and Pi-types;
    * respond with seeking instantiations and adding variables for the last two cases.
    *
    * @param goal type to resolve
    */
  case class ResolveGoal(goal: Typ[Term]) extends ReasonBackward

  sealed trait Decided {
    val statement: Typ[Term]
  }

  object Decided {
    implicit def decideMap: PostMaps[Decided] =
      PostMaps.empty[Decided] || ((p: Proved) => p) || ((c: Contradicted) => c)

    def asEither(d: Decided) : Either[Contradicted,Proved] = d match {
      case p @ Proved(statement, proofOpt) => Right(p)
      case c @ Contradicted(statement, contraOpt) => Left(c)
    }
  }

  case class Proved(statement: Typ[Term], proofOpt: Option[Term])
      extends Decided {
    proofOpt.foreach(proof => assert(proof.typ == statement))
  }

  trait PropagateProof {
    def propagate(proofs: Set[Term]): Option[Decided]
  }

  object PropagateProof {
    implicit def propsMap: PostMaps[PropagateProof] =
      PostMaps.empty[PropagateProof] || ((x: Consequence) => x) || (
          (x: Contradicts) => x
      ) || ((x: FromAll) => x) || ((x: FromAny) => x)
  }

  case class Consequence(
      premise: Typ[Term],
      conclusion: Typ[Term],
      proofMapOpt: Option[Term => Term]
  ) extends PropagateProof {
    def propagate(proofs: Set[Term]): Option[Proved] =
      proofs.find(_.typ == premise).map { proof =>
        Proved(conclusion, proofMapOpt.map(m => m(proof)))
      }
    assert(
      proofMapOpt.forall(pfMap => pfMap("hyp" :: premise).typ == conclusion)
    )
  }

  case class Contradicts(
      premise: Typ[Term],
      conclusion: Typ[Term],
      contraMapOpt: Option[Term => Term => Term]
  ) extends PropagateProof {
    def propagate(proofs: Set[HoTT.Term]): Option[Decided] =
      proofs.find(_.typ == premise).map { proof =>
        val contraOpt =
          contraMapOpt.map { contraMap =>
            contraMap(proof)
          }
        Contradicted(conclusion, contraOpt)
      }

    contraMapOpt.foreach(
      contraMap =>
        assert(contraMap("assume" :: premise)("also" :: conclusion).typ == Zero)
    )
  }

  case class Contradicted(
      statement: Typ[Term],
      contraOpt: Option[Term => Term]
  ) extends Decided {
    for {
      contra <- contraOpt
    } assert(contra("assume" :: statement).typ == Zero)
  }

  case class RepresentationMap(
      rep: Map[GeneratorVariables.Variable[_], Vector[Double]]
  )

  case class TimeLimit(duration: scala.concurrent.duration.FiniteDuration)

  case class HaltIf(condition: Unit => Boolean)

  case class Refine(scale: Double)

  case class OptimizeGenerators(damping: Double)

  case class LocalOptimizeGenerators(
      hW: Double,
      klW: Double,
      smoothing: Double,
      damping: Double
  )

  case class OptimalInitial(
      terms: FiniteDistribution[Term],
      hW: Double,
      klW: Double,
      smoothing: Double,
      damping: Double
  )

  case object GenerateTypes

  case object GenerateTerms

  case class SeekInstances[U <: Term with Subs[U], V <: Term with Subs[V]](
      typ: Typ[U],
      val variable: U,
      goalFamily: Typ[Term]
  )

  case class Instance[U <: Term with Subs[U]](term: U, typ: Typ[U]) {
    assert(term.typ == typ)
  }

  case class FromAll(typs: Vector[Typ[Term]], conclusion: Typ[Term])
      extends PropagateProof {
    def propagate(proofs: Set[HoTT.Term]): Option[Proved] =
      if (typs.toSet.subsetOf(proofs.map(_.typ))) Some(Proved(conclusion, None))
      else None
  }

  case class FromAny(
      typs: Vector[Typ[Term]],
      conclusion: Typ[Term],
      exhaustive: Boolean
  ) extends PropagateProof {
    def propagate(proofs: Set[HoTT.Term]): Option[Decided] =
      if (typs.toSet.intersect(proofs.map(_.typ)).nonEmpty)
        Some(Proved(conclusion, None))
      else if (exhaustive && typs.toSet.subsetOf(proofs.map(_.typ)))
        Some(Contradicted(conclusion, None))
      else None
  }

  @annotation.tailrec
  def propagateProofs(
      props: Set[PropagateProof],
      decisions: Set[Decided]
  ): Set[Decided] = {
    val proofs = decisions.collect {
      case Proved(statement, proofOpt) =>
        proofOpt.getOrElse("proved" :: statement)
    }

    val offspring: Set[Decided] =
      for {
        p  <- props
        pf <- p.propagate(proofs)
      } yield pf

    if (offspring.nonEmpty) propagateProofs(props, decisions union offspring)
    else decisions
  }

  def derivedProofs(
      props: Set[PropagateProof],
      decisions: Set[Decided]
  ): Set[Decided] = propagateProofs(props, decisions) -- decisions

  case class AddVariable(typ: Typ[Term])

  case class SplitFocus(number: Int, scale: Double)
}
