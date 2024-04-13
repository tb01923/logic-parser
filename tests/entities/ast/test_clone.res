open Test

test("/entities/ast/clone.res/clone", () => {
    open StringRepresentation

    // alternate alphabet
    let hm = Belt.HashMap.String.make(~hintSize=10)
    Belt.HashMap.String.set(hm, "p", 0)
    Belt.HashMap.String.set(hm, "q", 1)

    let ast = Ast.makeConjunction( Ast.makeVariable("a"), Ast.makeVariable("b"))

    // test the same AST / proposition, debuinj and variable should be equal
    Assert.Ast.isEqualByName("clone(" ++ printImplicit(ast) ++") == " ++ printImplicit(ast), ast, Clone.clone(ast))
    Assert.Ast.isEqualByDeBruinj("clone(" ++ printImplicitDeBruinj(ast) ++") == " ++ printImplicitDeBruinj(ast), 
        ast, Clone.clone(ast))

    // test the same AST / Proposition cloned into a new alphabet, debruinj should be equal but variables different
    let clonedNewAlpha = Clone.clone(ast, ~targetAlphabet=hm) 
    Assert.Ast.isNotEqualByName("clone(" ++ printImplicit(ast) ++") != " ++ printImplicit(ast), 
        ast, clonedNewAlpha)
    Assert.Ast.isEqualByDeBruinj("clone(" ++ printImplicitDeBruinj(ast) ++") == " ++ printImplicitDeBruinj(ast), 
        ast, clonedNewAlpha)
})

test("/entities/ast/clone.res/invertHashMap", ()=>{
    
    let hm = Belt.HashMap.String.make(~hintSize=2)
    Belt.HashMap.String.set(hm, "p", 0)
    Belt.HashMap.String.set(hm, "q", 1)

    let hm2 = Belt.HashMap.Int.make(~hintSize=2)
    Belt.HashMap.Int.set(hm2, 0, "p")
    Belt.HashMap.Int.set(hm2, 1, "q")
    
    Assert.HashMapIntString.isHashMapEqual("hashmap should invert from { <k : v> } to { <v : k> }",  Clone.invertHashMap(hm), hm2)
})