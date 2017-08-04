package trepplein

import org.specs2.mutable._

class NatTest extends Specification {
  val nat     = Const("nat", Vector())
  val natZero = Const(Name("nat", "zero"), Vector())
  val natSucc = Const(Name("nat", "succ"), Vector())

  val natAdd = Const(Name("nat", "add"), Vector())
  def addDef = {
    val x = LocalConst(Binding("x", nat, BinderInfo.Default))
    val y = LocalConst(Binding("y", nat, BinderInfo.Default))
    Lam(x,
        Apps(
          Const(Name("nat", "rec"), Vector(1)),
          Lam(y, nat),
          x,
          Lam(y, natSucc)
        ))
  }

  def env_ =
    Environment.default
      .addNow(
        IndMod(
          InductiveType(nat.name, Vector(), Sort(1)),
          0,
          Vector(natZero.name -> nat, natSucc.name -> (nat -->: nat))
        ))
      .addNow(DefMod(
        Definition(natAdd.name, Vector(), nat -->: nat -->: nat, addDef)))

  def numeral(n: Int): Expr =
    if (n == 0) Const(Name("nat", "zero"), Vector())
    else App(Const(Name("nat", "succ"), Vector()), numeral(n - 1))

  def tc = new TypeChecker(env_)

  // "add" in {
  //   val t = Apps(natAdd, numeral(2), numeral(4))
  //   tc.checkDefEq(tc.infer(t), nat) must beNone
  //   tc.checkDefEq(t, numeral(6)) must beNone
  //   tc.checkDefEq(t, numeral(7)) must beSome
  // }
}
