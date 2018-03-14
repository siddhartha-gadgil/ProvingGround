val VecAIndGen = Some(IndexedConstructorSeqDom.Cons(HoTT.Name("nil"), IndexedConstructorShape.IndexedIdShape(TypFamilyPtn.FuncTypFamily(Type.symbObj(Name("Nat")), TypFamilyPtn.IdTypFamily.byTyp((Type.symbObj(Name("Nat")) ->: Type).symbObj(Name("Vec"))(Type.symbObj(Name("Nat")).symbObj(Name("0"))))), {
  import shapeless._
  Type.symbObj(Name("Nat")).symbObj(Name("0")) :: HNil
}), IndexedConstructorSeqDom.Cons(HoTT.Name("cons"), {
  val x = Type.symbObj(Name("Nat")).symbObj(Name("$afdbz"))
  x ~>>: IndexedConstructorShape.IndexedCnstFuncConsShape(Type.symbObj(Name("A")), IndexedConstructorShape.IndexedIndexedFuncConsShape(IndexedIterFuncShape.IdIterShape(TypFamilyPtn.FuncTypFamily(Type.symbObj(Name("Nat")), TypFamilyPtn.IdTypFamily.byTyp((Type.symbObj(Name("Nat")) ->: Type).symbObj(Name("Vec"))(Type.symbObj(Name("Nat")).symbObj(Name("$afdbz"))))), {
    import shapeless._
    Type.symbObj(Name("Nat")).symbObj(Name("$afdbz")) :: HNil
  }), IndexedConstructorShape.IndexedIdShape(TypFamilyPtn.FuncTypFamily(Type.symbObj(Name("Nat")), TypFamilyPtn.IdTypFamily.byTyp((Type.symbObj(Name("Nat")) ->: Type).symbObj(Name("Vec"))((Type.symbObj(Name("Nat")) ->: Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(Type.symbObj(Name("Nat")).symbObj(Name("$afdbz")))))), {
    import shapeless._
    (Type.symbObj(Name("Nat")) ->: Type.symbObj(Name("Nat"))).symbObj(Name("succ"))(Type.symbObj(Name("Nat")).symbObj(Name("$afdbz"))) :: HNil
  }), {
    import shapeless._
    Type.symbObj(Name("Nat")).symbObj(Name("$afdbz")) :: HNil
  }))
}, IndexedConstructorSeqDom.Empty((Type.symbObj(Name("Nat")) ->: Type).symbObj(Name("Vec")), TypFamilyPtn.FuncTypFamily(Type.symbObj(Name("Nat")), TypFamilyPtn.IdTypFamily.byTyp((Type.symbObj(Name("Nat")) ->: Type).symbObj(Name("Vec"))(Type.symbObj(Name("Nat")).symbObj(Name("$afdca")))))))))