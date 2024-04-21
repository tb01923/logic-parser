open Test

test("/entities/ast/equality.res/binOpEquals", () => {
    let a = Ast.Variable("" ,"a")
    let b = Ast.Variable("" ,"b")
    let c = Ast.Variable("" ,"c")
    let notT = Ast.UnaryOperation("", Ast.Negation, Ast.Value("", true))
    let aAndB = Ast.BinaryOperation("", Ast.Conjunction, a, b)
    let aOrB = Ast.BinaryOperation("", Ast.Disjunction, a, b)
    let aImplB = Ast.BinaryOperation("", Ast.Conditional, a, b)
    let aBiImplB = Ast.BinaryOperation("", Ast.BiConditional, a, b)
    let aEquivA = Ast.BinaryOperation("", Ast.Equivalence, a, a)
    let abstractAandB = Ast.Abstraction("", "c", aAndB)

    let binOpEqualsTest = (a, bArr) => {
        let aName = StringRepresentation.getBinOpName(a)
        
        Assert.Boolean.isTrue("binOpEquals(" ++ aName ++ ", " ++ aName ++ ") should be true",  
            Equality.binOpEquals(a, a))

        Belt.Array.forEach(bArr, b => {
            let bName = StringRepresentation.getBinOpName(b)    
            Assert.Boolean.isFalse("binOpEquals(" ++ aName ++ "," ++ bName ++ ") should be false",  
                Equality.binOpEquals(a, b))
        })
    }

    binOpEqualsTest(Ast.Conjunction, [Ast.Disjunction, Ast.Conditional, Ast.BiConditional, Ast.Equivalence])
    binOpEqualsTest(Ast.Disjunction, [Ast.Conjunction, Ast.Conditional, Ast.BiConditional, Ast.Equivalence])
    binOpEqualsTest(Ast.Conditional, [Ast.Conjunction, Ast.Disjunction, Ast.BiConditional, Ast.Equivalence])
    binOpEqualsTest(Ast.BiConditional, [Ast.Conjunction, Ast.Disjunction, Ast.Conditional, Ast.Equivalence])
    binOpEqualsTest(Ast.Equivalence, [Ast.Conjunction, Ast.Disjunction, Ast.Conditional, Ast.BiConditional])
})