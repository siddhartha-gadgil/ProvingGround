/**
 * This is work towards automated theorem proving based on learning, using
 * homotopy type theory (HoTT) as foundations and natural language processing.
 *
 * The implementation of homotopy type theory is split into:
 *   - the object [[HoTT]] with terms, types, functions and dependent functions, pairs etc
 *   - the package [[induction]] with general inductive types and recursion/induction on these.
 *
 * The [[learning]] package has the code for learning.
 *
 * Scala code, including the ``spire`` library, is integrated with homotopy type theory
 * in the [[functionfinder]] package
 *
 * We have implemented a functor based approach to translation in the [[translation]]
 * package, used for ``nlp`` as well as ``serialization`` and ``parsing``.
 *
 * The [[library]] package is contains basic structures implemented in HoTT.
 */
package object provingground{}
