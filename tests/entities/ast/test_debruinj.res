open Test

test("/entities/ast/debruinj.res/getDebruinjIndices", () => {
    let t = Ast.Value("", true)
    let a = Ast.Variable("" ,"a")
    let b = Ast.Variable("" ,"b")
    let notT = Ast.UnaryOperation("", Ast.Negation, Ast.Value("", true))
    let aAndB = Ast.BinaryOperation("", Ast.Conjunction, a, b)
    let bAndA = Ast.BinaryOperation("", Ast.Conjunction, b, a)
    let aOrB = Ast.BinaryOperation("", Ast.Disjunction, a, b)
    let aImplB = Ast.BinaryOperation("", Ast.Conditional, a, b)
    let aBiImplB = Ast.BinaryOperation("", Ast.BiConditional, a, b)
    let aEquivA = Ast.BinaryOperation("", Ast.Equivalence, a, a)
    let abstractAandB = Ast.Abstraction("", "c", aAndB)

    
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
    ()
    
})