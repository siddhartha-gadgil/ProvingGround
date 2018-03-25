
val id = lambda(Type.symbObj(Name("A")))(lmbda(Type.symbObj(Name("A")).symbObj(Name("a")))(Type.symbObj(Name("A")).symbObj(Name("a"))))
val mp = lambda(Type.symbObj(Name("A")))(lambda(Type.symbObj(Name("B")))(lmbda(Type.symbObj(Name("A")).symbObj(Name("a")))(lmbda(FuncTyp(Type.symbObj(Name("A")), Type.symbObj(Name("B"))).symbObj(Name("_:A->B")))(FuncTyp(Type.symbObj(Name("A")), Type.symbObj(Name("B"))).symbObj(Name("_:A->B"))(Type.symbObj(Name("A")).symbObj(Name("a")))))))
