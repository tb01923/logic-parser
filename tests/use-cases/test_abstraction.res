open Test

let testAbstraction = (s0, s1) => {

    let s1Strings = Belt.Set.reduce(s1, "", (acc, element) =>
         acc ++ StringRepresentation.printImplicit(element) ++ " | "
    )

    Assert.Boolean.isTrue(
        "`a and b and c and d` should have these { " ++ s1Strings ++ "} abstractions", 
        Belt.Set.eq(s0, s1))

    
}

test("/use-cases/abstaction.res/getAbstractions count and negative tests", () => {
    let abstractions  = "a and b and c and d"
    ->Parser.parse
    ->Abstraction.getAbstractions
    
    Assert.Basics.isIntEqual("`a and b and c and d` should have 4 abstractions", Belt.Array.length(abstractions), 4)

    Assert.Ast.isNotEqualByName("`a and b and c and d` abstraction[3] should not be `[h/(a ∧ (b ∧ (c ∧ d)))]`", 
        Belt.Array.getExn(abstractions, 3), Parser.parse("[h/(a ∧ (b ∧ (c ∧ d)))]"))
})

test("/use-cases/abstaction.res/getAbstractions positive tests", () => {
    let abstractions  = "a and b and c and d"
    ->Parser.parse
    ->Abstraction.getAbstractions

    let s1 = Belt.Set.make(~id=module(Equality.PropositionCompare))
            -> Belt.Set.add(Parser.parse("a and b and c and d"))
            -> Belt.Set.add(Parser.parse("(a ∧ (b ∧ [e/(c ∧ d)]))"))
            -> Belt.Set.add(Parser.parse("(a ∧ [f/(b ∧ (c ∧ d))])"))
            -> Belt.Set.add(Parser.parse("[g/(a ∧ (b ∧ (c ∧ d)))]"))

    let s2 = Belt.Set.fromArray(~id=module(Equality.PropositionCompare), abstractions)

    testAbstraction(s1, s2)
})