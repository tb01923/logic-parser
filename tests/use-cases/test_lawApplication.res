open Test

test("/use-cases/lawApplication.res/identifyLaws", () => {
    let ast  = Parser.parse("a and b or c or d")
    // Js.Console.log()
    
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