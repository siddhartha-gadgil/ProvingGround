import library._, Nats._ 
val DoubleEvenDOTpf = ({
  val rxyz = NatInd.induc(lmbda(Type.symbObj(Name("Nat")).symbObj(Name("n")))(FuncTyp(Type.symbObj(Name("Nat")), Type).symbObj(Name("isEven"))(({
    val rxyz = NatInd.rec(Type.symbObj(Name("Nat")))
    rxyz
  })(Type.symbObj(Name("Nat")).symbObj(Name("0")))(lmbda(Type.symbObj(Name("Nat")).symbObj(Name("_")))(lmbda(Type.symbObj(Name("Nat")).symbObj(Name("n")))(FuncTyp(Type.symbObj(Name("Nat")), Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(FuncTyp(Type.symbObj(Name("Nat")), Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(Type.symbObj(Name("Nat")).symbObj(Name("n")))))))(Type.symbObj(Name("Nat")).symbObj(Name("n"))))))
  rxyz
})(FuncTyp(Type.symbObj(Name("Nat")), Type).symbObj(Name("isEven"))(Type.symbObj(Name("Nat")).symbObj(Name("0"))).symbObj(Name("0even")))(lambda(Type.symbObj(Name("Nat")).symbObj(Name("n")))(lmbda(FuncTyp(Type.symbObj(Name("Nat")), Type).symbObj(Name("isEven"))(({
  val rxyz = NatInd.rec(Type.symbObj(Name("Nat")))
  rxyz
})(Type.symbObj(Name("Nat")).symbObj(Name("0")))(lmbda(Type.symbObj(Name("Nat")).symbObj(Name("_")))(lmbda(Type.symbObj(Name("Nat")).symbObj(Name("n")))(FuncTyp(Type.symbObj(Name("Nat")), Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(FuncTyp(Type.symbObj(Name("Nat")), Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(Type.symbObj(Name("Nat")).symbObj(Name("n")))))))(Type.symbObj(Name("Nat")).symbObj(Name("n")))).symbObj(Name("isEven(double(n))")))(piDefn(Type.symbObj(Name("Nat")).symbObj(Name("n")))(FuncTyp(FuncTyp(Type.symbObj(Name("Nat")), Type).symbObj(Name("isEven"))(Type.symbObj(Name("Nat")).symbObj(Name("n"))), FuncTyp(Type.symbObj(Name("Nat")), Type).symbObj(Name("isEven"))(FuncTyp(Type.symbObj(Name("Nat")), Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(FuncTyp(Type.symbObj(Name("Nat")), Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(Type.symbObj(Name("Nat")).symbObj(Name("n"))))))).symbObj(Name("_+2even"))(({
  val rxyz = NatInd.rec(Type.symbObj(Name("Nat")))
  rxyz
})(Type.symbObj(Name("Nat")).symbObj(Name("0")))(lmbda(Type.symbObj(Name("Nat")).symbObj(Name("_")))(lmbda(Type.symbObj(Name("Nat")).symbObj(Name("n")))(FuncTyp(Type.symbObj(Name("Nat")), Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(FuncTyp(Type.symbObj(Name("Nat")), Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(Type.symbObj(Name("Nat")).symbObj(Name("n")))))))(Type.symbObj(Name("Nat")).symbObj(Name("n"))))(FuncTyp(Type.symbObj(Name("Nat")), Type).symbObj(Name("isEven"))(({
  val rxyz = NatInd.rec(Type.symbObj(Name("Nat")))
  rxyz
})(Type.symbObj(Name("Nat")).symbObj(Name("0")))(lmbda(Type.symbObj(Name("Nat")).symbObj(Name("_")))(lmbda(Type.symbObj(Name("Nat")).symbObj(Name("n")))(FuncTyp(Type.symbObj(Name("Nat")), Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(FuncTyp(Type.symbObj(Name("Nat")), Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(Type.symbObj(Name("Nat")).symbObj(Name("n")))))))(Type.symbObj(Name("Nat")).symbObj(Name("n")))).symbObj(Name("isEven(double(n))"))))))
