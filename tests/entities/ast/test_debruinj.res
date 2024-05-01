open Test

test("/entities/ast/debruinj.res/getDebruinjIndices", () => {
    let a = Ast.Variable("" ,"a")
    let b = Ast.Variable("" ,"b")
    let aAndB = Ast.BinaryOperation("", Ast.Conjunction, a, b)
    let bAndA = Ast.BinaryOperation("", Ast.Conjunction, b, a)

    
    let hm1 = Belt.HashMap.String.make(~hintSize=2)
    Belt.HashMap.String.set(hm1, "a", 0)
    Belt.HashMap.String.set(hm1, "b", 1)
    Assert.HashMapStringInt.isHashMapEqual("(a and b) should have debruinj {a:0, b:1}",  Debruinj.getDebruinjIndices(aAndB), hm1)

    let hm2 = Belt.HashMap.String.make(~hintSize=2)
    Belt.HashMap.String.set(hm2, "a", 1)
        Belt.HashMap.String.set(hm2, "b", 0)
    Assert.HashMapStringInt.isHashMapEqual("(b and a) should have debruinj {a:1, b:0}",  Debruinj.getDebruinjIndices(bAndA), hm2)

})

test("/entities/ast/debruinj.res/getNextSymbol", () => {
    let hm1 = Belt.HashMap.String.make(~hintSize=2)
    Belt.HashMap.String.set(hm1, "a", 0)
    Belt.HashMap.String.set(hm1, "b", 1)
    Assert.Basics.isStringEqual("{a:0, b:1} should have 'c' as next symbol", "c", Debruinj.getNextSymbol(hm1))
    Belt.HashMap.String.set(hm1, "d", 2)
    Assert.Basics.isStringEqual("{a:0, b:1, d:2} should have 'e' as next symbol", "e", Debruinj.getNextSymbol(hm1))
    let hm2 = Belt.HashMap.String.make(~hintSize=2)
    Assert.Basics.isStringEqual("{} should have 'a' as next symbol", "a", Debruinj.getNextSymbol(hm2))
})

test("/entities/ast/debruinj.res/addSymbolToIndices", () => {
    let hm1 = Belt.HashMap.String.make(~hintSize=2)
    Belt.HashMap.String.set(hm1, "a", 0)
    Belt.HashMap.String.set(hm1, "b", 1)

    let hm2 = Belt.HashMap.String.make(~hintSize=3)
    Belt.HashMap.String.set(hm2, "a", 0)
    Belt.HashMap.String.set(hm2, "b", 1)
    Belt.HashMap.String.set(hm2, "c", 2)

    Assert.HashMapStringInt.isHashMapEqual("{a:0, b:1} addSymbolToIndices('c') should equal {a:0, b:1, c:2}", 
        Debruinj.addSymbolToIndices(hm1, "c"), hm2)
    Assert.HashMapStringInt.isHashMapEqual("{a:0, b:1, c:2} addSymbolToIndices('c') should equal {a:0, b:1, c:2}", 
        Debruinj.addSymbolToIndices(hm2, "c"), hm2)
    Assert.HashMapStringInt.isHashMapEqual("{a:0, b:1, c:2} addSymbolToIndices('b') should equal {a:0, b:1, c:2}", 
        Debruinj.addSymbolToIndices(hm2, "b"), hm2)

    let hm3 = Belt.HashMap.String.make(~hintSize=2)
    Belt.HashMap.String.set(hm3, "b", 0)
    Assert.HashMapStringInt.isHashMapEqual("{} addSymbolToIndices('b') should equal {b1}", 
        Debruinj.addSymbolToIndices(Belt.HashMap.String.make(~hintSize=2), "b"), hm3)
})

test("/entities/ast/debruinj.res/getNextDebuinjIndex", () => {
    let hm1 = Belt.HashMap.String.make(~hintSize=2)
    Belt.HashMap.String.set(hm1, "a", 0)
    Belt.HashMap.String.set(hm1, "b", 1)
    Assert.Basics.isIntEqual("{a:0, b:1} getNextDebuinjIndex should return 2", 2, Debruinj.getNextDebuinjIndex(hm1))
    Belt.HashMap.String.set(hm1, "d", 3)
    Assert.Basics.isIntEqual("{a:0, b:1, d:3} getNextDebuinjIndex should return 4", 4, Debruinj.getNextDebuinjIndex(hm1))
    let hm2 = Belt.HashMap.String.make(~hintSize=2)
    Assert.Basics.isIntEqual("{} getNextDebuinjIndex should return 0", 0, Debruinj.getNextDebuinjIndex(hm2))
})
