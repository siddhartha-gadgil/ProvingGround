package provingground

import provingground.Collections._

import provingground.LearningSystem._

object RandomWords {
  /*
   * A symbol is either an element of E wrapped, a word in E or an operation on E.
   */
  class Symbol[E]

  trait Word[E] extends Symbol[E] {
    def value: E
  }

  case class Letter[E](value: E) extends Word[E]

  case class BinOpSym[E, N](name: N, op: (E, E) => E) extends Symbol[E] {
    self =>
    def apply(a: E, b: E) = op(a, b)

    def apply(a: Word[E], b: Word[E]) = BinOpWord(this, a, b)

    def assoc(a: Word[E], b: Word[E]) = (a, b) match {
      case (x: AssocWord[E, N], y: AssocWord[E, N])
          if x.op == self && y.op == self =>
        AssocWord(self, x.elems ::: y.elems)
      case (x: AssocWord[E, N], y) if x.op == self =>
        AssocWord(self, x.elems :+ y)
      case (x, y: AssocWord[E, N]) if y.op == self =>
        AssocWord(self, x +: y.elems)
      case _ => apply(a, b)
    }
  }

  case class UnOpSym[E, N](name: N, op: E => E) extends Symbol[E] {
    def apply(a: E) = op(a)

    def apply(a: Word[E]) = UnOpWord(this, a)
  }

  case class BinOpWord[E, N](op: BinOpSym[E, N],
                             first: Word[E],
                             second: Word[E])
      extends Word[E] {
    def value = op(first.value, second.value)
  }

  case class UnOpWord[E, N](op: UnOpSym[E, N], elem: Word[E]) extends Word[E] {
    def value = op(elem.value)
  }

  trait AggregateWord[E] extends Word[E] {
    def terms: Traversable[Word[E]]
  }

  case class AssocWord[E, N](op: BinOpSym[E, N], elems: List[Word[E]])
      extends AggregateWord[E] {
    def value = elems map (_.value) reduce (op(_, _))

    def simplify = elems match {
      case List(e) => e
      case _       => this
    }

    def terms =
      (for (n <- 1 to elems.length - 1)
        yield
          List(AssocWord(op, elems take n), AssocWord(op, elems drop n))).flatten
        .map(_.simplify)
  }

  def recprob[E](m: FiniteDistribution[Symbol[E]])(w: Word[E]): Double =
    w match {
      case l: Letter[_] => m(l)
      case word @ BinOpWord(op, first, second) =>
        m(op) * m(first) * m(second) + m(word)
      case word @ UnOpWord(op, elem) => m(op) * m(elem) + m(word)
      case agg: AggregateWord[_]     => (agg.terms map (recprob(m)(_))).sum
    }

  def partial[E](m: FiniteDistribution[Symbol[E]])(w: Word[E],
                                                   x: Symbol[E]): Double =
    (x, w) match {
      case (a, b) if a == b         => 1
      case (UnOpWord(`x`, elem), _) => m(x) * partial(m)(elem, x) + m(elem)
      case (UnOpWord(op, elem), _)  => m(op) * partial(m)(elem, x)
      case (BinOpWord(`x`, first, second), _) =>
        m(x) * partial(m)(first, x) * m(second) +
          m(x) * partial(m)(second, x) * m(first) + m(first) * m(second)
      case (BinOpWord(op, first, second), _) =>
        m(op) * partial(m)(first, x) * m(second) +
          m(op) * partial(m)(second, x) * m(first)
      case agg: AggregateWord[_] =>
        (agg.asInstanceOf[AggregateWord[E]].terms map (partial(m)(_, x))).sum
      case _ => 0.0
    }

  def evolution[E](supp: Set[Word[E]]) = {
    def pushforward(m: FiniteDistribution[Symbol[E]]) =
      supp map ((w) => Weighted(w, recprob(m)(w)))

    def pushmass(m: FiniteDistribution[Symbol[E]]) =
      (pushforward(m) map (_.weight)).sum

    def forward(m: FiniteDistribution[Symbol[E]]) =
      FiniteDistribution((pushforward(m) map (_.scale(1.0 / pushmass(m)))))

    /*
     * There is no assumption that vector is normalized, so feedback does not have to do this.
     */
    def back(m: FiniteDistribution[Symbol[E]])(
        vect: FiniteDistribution[Word[E]]) = {
      val support = (m.support ++ supp).toSet

      def xterms(x: Symbol[E]) =
        (for (Weighted(w, p) <- pushforward(m)) yield p * partial(m)(w, x)).sum

      val eachshift = (support map (xterms(_))).sum / (m.support.size)

      def xshift(x: Symbol[E]) =
        if (m.support contains x) xterms(x) - eachshift else xterms(x)

      FiniteDistribution((support map ((x) => Weighted(x, xshift(x)))).toSet)
    }

    AdjDiffbleFunction(forward)(back)
  }

  def relEntropy[E](bg: FiniteDistribution[E], d: FiniteDistribution[E]) = {
    (for (Weighted(x, w) <- bg.pmf if d(x) > 0)
      yield (w * math.log(1 / d(x)))).sum
  }

  def entropyFeedback[E](bg: FiniteDistribution[E],
                         d: FiniteDistribution[E]) = {
    FiniteDistribution(
      for (Weighted(x, w) <- bg.pmf if d(x) > 0) yield (Weighted(x, w / d(x))))
  }

  def simpleLearn[E](supp: Set[Word[E]],
                     target: FiniteDistribution[Word[E]],
                     epsilon: Double)(init: FiniteDistribution[Symbol[E]]) = {
    val estimate = evolution(supp).func(init)
    (init ++ evolution(supp).grad(init)(
      entropyFeedback(target, estimate) * epsilon)).normalized()
  }
}
