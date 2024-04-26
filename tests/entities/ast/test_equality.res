open Test

test("/entities/ast/equality.res/binOpEquals", () => {

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

test("/entities/ast/equality.res/unOpEquals", () => {
    let a = Ast.Negation
    let aName = StringRepresentation.getUnOpName(a)

    Assert.Boolean.isTrue("unOpEquals(" ++ aName ++ ", " ++ aName ++ ") should be true",  
            Equality.unOpEquals(a, a))
})

test("/entities/ast/equality.res/byName", () => {
    let a = Ast.Variable("" ,"a")
    let b = Ast.Variable("" ,"b")
    let c = Ast.Variable("" ,"c")
    let d = Ast.Variable("" ,"d")
    let aAndB = Ast.BinaryOperation("", Ast.Conjunction, a, b)
    let cAndD = Ast.BinaryOperation("", Ast.Conjunction, c, d)
    let aOrB = Ast.BinaryOperation("", Ast.Disjunction, a, b)
    
    let abstractAandBasE = Ast.Abstraction("", "e", aAndB)
    let abstractAandBasF = Ast.Abstraction("", "f", aAndB)
    let abstractCandDasE = Ast.Abstraction("", "e", cAndD)

    let test = (a, b, compare, label) => {
        let aName = StringRepresentation.printImplicit(a)
        let bName = StringRepresentation.printImplicit(b)

        compare("byName(" ++ aName ++ ", " ++ bName ++ ") " ++ label,  
            Equality.byName(a, b))
    }

    test(aAndB, aAndB, Assert.Boolean.isTrue, "should be true")
    test(aAndB, aOrB, Assert.Boolean.isFalse, "should be false")
    test(aAndB, cAndD, Assert.Boolean.isFalse, "should be false")
    test(abstractAandBasE, abstractAandBasE, Assert.Boolean.isTrue, "should be true")
    test(abstractAandBasE, abstractAandBasF, Assert.Boolean.isFalse, "should be false")

    // this is true even though the variables within the abstraction are different byName 
    //      (within).  byName should only look at the label of the Abstraction, not within 
    //      the Abstraction (think: "shallow")
    test(abstractAandBasE, abstractCandDasE, Assert.Boolean.isTrue, "should be true")  
})


test("/entities/ast/equality.res/byDeBruinj", () => {
    let a = Ast.Variable("" ,"a")
    let b = Ast.Variable("" ,"b")
    let c = Ast.Variable("" ,"c")
    let d = Ast.Variable("" ,"d")
    let aAndB = Ast.BinaryOperation("", Ast.Conjunction, a, b)
    let cAndD = Ast.BinaryOperation("", Ast.Conjunction, c, d)
    let aOrB = Ast.BinaryOperation("", Ast.Disjunction, a, b)
    
    let abstractAandBasE = Ast.Abstraction("", "e", aAndB)
    let abstractAandBasF = Ast.Abstraction("", "f", aAndB)
    let abstractCandDasE = Ast.Abstraction("", "e", cAndD)

    let test = (a, b, compare, label) => {
        let aName = StringRepresentation.printImplicit(a)
        let bName = StringRepresentation.printImplicit(b)

        compare("byDeBruinj(" ++ aName ++ ", " ++ bName ++ ") " ++ label,  
            Equality.byDebruinj(a, b))
    }

    test(aAndB, aAndB, Assert.Boolean.isTrue, "should be true")
    test(aAndB, aOrB, Assert.Boolean.isFalse, "should be false")
    test(aAndB, cAndD, Assert.Boolean.isTrue, "should be true")
    test(abstractAandBasE, abstractAandBasE, Assert.Boolean.isTrue, "should be true")
    test(abstractAandBasE, abstractAandBasF, Assert.Boolean.isTrue, "should be true")
    test(abstractAandBasE, abstractCandDasE, Assert.Boolean.isTrue, "should be true")  
})

test("/entities/ast/equality.res/byAbstractionResolution", () => {
    ()
})