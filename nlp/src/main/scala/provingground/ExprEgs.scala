package provingground

import StanfordParser._

import TreeToMath._

/**
  * Examples to experiment and illustrate expression level NLP parsing
  * Issues revealed:
  *  - Cardinals should be treated as determiners
  *  - determiners in a phrase e.g. 'divides some ...' need special rules
  *  - useless 'then'
  *  - primes is tagged as a verb, not a noun
  *  - in 'are represented by ...', 'represented by ...'
  *  - copula followed by an adjectival phrase is mishandled
  *  - TeX fragment that is part of a word gives error.
  */
object ExprEgs {
  val assertions =
    Vector(
      "Every natural number is greater than $0$", //parsed
      "Every natural number $n$, where $n$ is greater than $1$, is divisible by a prime number", //parsed
      "Every natural number $n$ which is greater than $1$ is divisible by a prime number", //parsed
      "if a prime number $p$ divides the product of $m$ and $n$,  $p$ divides one of $m$ and $n$",
      "if a prime number $p$ divides the product of $m$ and $n$,  $p$ divides $m$ or $n$", //parsed
      "if a prime number $p$ divides $mn$, $p$ divides $m$ or $n$", //parsed
      "Every natural number which is greater than $1$ is divisible by a prime number",
      "Every natural number $n$, which is greater than $1$, is divisible by a prime number", //parsed
      // "Six is not the sum of two distinct primes",
      "$6$ is not the sum of two distinct prime numbers", // 'primes' causes trouble
      "Every natural number is greater than $0$",
      // "The image of $Z$ in $K$ is an integral domain, hence isomorphic to $Z$ or $Z/p$, where $p$ is a prime",
      "If $G/H$ is cyclic, then $G$ is abelian", // parsed
      "If $G/H$ is cyclic, $G$ is abelian", //parsed
      "$\\ker \\phi$ is the set of all $a\\in A$ that map to an element in $B$",
      "$6$ is not the square of a prime", //parsed
      "$6$ is not the square of all prime numbers", //parsed,
      "$6$ is not the sum of distinct prime numbers",
      "$6$ is not the square of all primes",
      // "An abelian group is finitely generated if and only if the corresponding $Z$-module is finitely generated",
      "$G$ is solvable if there exists $n in \\N$ such that $G^{(n)}=1$", //parsed
      "there are $n,m\\in \\Z$ such that $xH = (gH)^n = g^nH$",
      // "Two quadratic forms over $k$ are equivalent if and only if they have the same rank",
      "Two quadratic forms over $k$ are equivalent if and only if they have the same rank", //experiment iff -> and
      "Two quadratic forms over $k$ are equivalent if and only if they have the same rank, same discriminant and same invariant $\\epsilon$",
      "The discriminant of $g$ is equal to $d/a$", //parsed
      // "The number of elements of $k/k^2$ which are represented by $f$ is equal to $1$",
      // "The number of elements of $k/k^2$ which are represented by $f$ is equal to $1$ if $n=1$, to $2^r -1$ if $n=3$, and to $2^r$ if $n=4$",
      "if $p$ is a prime number, the form deduced from $f$ by reduction modulo $p$ has a non-trivial root",
      "if $p$ is a prime number, the form deduced from $f$ by reduction modulo $p$ has a non-trivial zero, and this zero can be lifted to a p-adic zero",
      "the quadratic form $f$ represents zero in all the $Q_p$, and also in $R$",
      "if two diagrams $D_1$ and $D_2$ are related by a chain of Reidemeister moves, the complexes of graded abelian groups $C(D_1)$ and $C(D_2)$ are equivalent and homology groups $H(D_1)$ and $H(D_2)$ are isomorphic",
      "$AB \\subgroup G$ if and only if $AB = BA$", //parsed
      "$AB \\subgroup G$ and $AB = BA$", //experiment iff -> and; parsed
      "$[A,B] = \\{e\\}$ if and only if $ab = ba, \\forall a \\in A, b \\in B$ if and only if $A \\subgroup C_G(B)$ if and only if $B \\subgroup C_G(A)$"
    )

  def parse(s: String) = mathExpr(texParse(s))

  def parseT(s: String) = mathExprTree(texParse(s))

  lazy val parsed =
    assertions.map((s) => s -> mathExpr(texParse(s))).filter(!_._2.isEmpty).toMap

  lazy val exprs =
    assertions.map((s) => s -> mathExprFormal()(texParse(s))).toMap
}
