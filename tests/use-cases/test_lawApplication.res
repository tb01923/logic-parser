open Test

test("/use-cases/lawApplication.res/identifyLaws: a and b or c or d", () => {
    let ast  = Parser.parse("a and b or c or d")

    let actual = LawApplication.identifyLaws(ast)

    let expected = [
        LawApplication.makeTransformation(
            Belt.Option.getExn(Laws.getLawByName("Commutative<and>")), 
            LawApplication.LHS, 
            Parser.parse("a and b")),

        LawApplication.makeTransformation(
            Belt.Option.getExn(Laws.getLawByName("Commutative<or>")), 
            LawApplication.LHS, 
            Parser.parse("c or d"))
    ]

    Assert.LawApplication.isTransformationsEqual(
        "actual matching laws for " ++ 
            StringRepresentation.printImplicit(ast) ++ 
            " should equal [Commutative<and>, Commutative<or>]", 
        actual, expected)
})

test("/use-cases/lawApplication.res/identifyLaws: a and b or p and q", () => {
    let ast  = Parser.parse("a and b or p and q")
        
    let actual = LawApplication.identifyLaws(ast)

    let expected = [
        LawApplication.makeTransformation(
            Belt.Option.getExn(Laws.getLawByName("Commutative<and>")), 
            LawApplication.LHS, 
            Parser.parse("a and b")),

        LawApplication.makeTransformation(
            Belt.Option.getExn(Laws.getLawByName("Commutative<and>")), 
            LawApplication.LHS, 
            Parser.parse("p and q"))
    ]

    Assert.LawApplication.isTransformationsEqual(
        "actual matching laws for " ++ 
            StringRepresentation.printImplicit(ast) ++ 
            " should equal [Commutative<and>, Commutative<and>]", 
        actual, expected)
})