package provingground.learning
import provingground._, HoTT._
import provingground.induction.ExstInducDefn

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
    * A local prover to use, could be to start things or after tuning.
    *
    * @param lp the local prover
    */
  case class UseProver(lp: LocalProver)

  /**
    * modifying parameters, for instance excluding islands, strengthening backward reasoning ertc
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
  case class UseLemma(lemma: Typ[Term], proof: Term, weight: Double)

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

  case class Consequence(
      premise: Typ[Term],
      conclusion: Typ[Term],
      proofMap: Term => Term
  ) {
    assert(proofMap("hyp" :: premise).typ == conclusion)
  }

  case class Contradicts(
      premise: Typ[Term],
      conclusion: Typ[Term],
      contraMap: Term => Term => Term
  ) {
    assert(contraMap("assume" :: premise)("also" :: conclusion).typ == Zero)
  }

  case class Proved(statement: Typ[Term], proof: Term) {
    assert(proof.typ == statement)
  }

  case class Contradicted(
      statement: Typ[Term],
      disProof: Term,
      contra: Term => Term => Term
  ) {
    assert(contra(disProof)("assume" :: statement).typ == Zero)
  }

  case class RepresenationMap(
      rep: Map[GeneratorVariables.Variable[_], Vector[Double]]
  )

  case class TimeLimit(duration: scala.concurrent.duration.FiniteDuration)

  case class HaltIf(condition: Unit => Boolean)

  case class Refine(scale: Double)

  case class OptimizeGenerators(damping: Double)

  case object GenerateTypes

  case object GenerateTerms

  case class SeekInstances[U <: Term with Subs[U]](
      typ: Typ[U],
      goal: Typ[Term],
      proof: U => Typ[Term]
  )

  case class Instance[U <: Term with Subs[U]](term: U, typ: Typ[U]) {
    assert(term.typ == typ)
  }

  case class FromAll(typs: Vector[Typ[Term]], conclusion: Typ[Term])

  case class FromAny(
      typs: Vector[Typ[Term]],
      conclusion: Typ[Term],
      exhaustive: Boolean
  )

  case class AddVariable(typ: Typ[Term])

  case class SplitFocus(number: Int, scale: Double)
}
