open Test

test("/use-cases/abstaction.res/getAbstractions", () => {
    let abstractions  = "a and b and c and d"
    ->Parser.parse
    ->Abstraction.getAbstractions

    Assert.Basics.isIntEqual("`a and b and c and d` should have 4 abstractions", Belt.Array.length(abstractions), 4)


    Assert.Ast.isEqualByName("`a and b and c and d` abstraction[0] should be `a and b and c and d`", 
        Belt.Array.getExn(abstractions, 0), Parser.parse("a and b and c and d"))

    Assert.Ast.isEqualByName("`a and b and c and d` abstraction[1] should be `(a ∧ (b ∧ [e/(c ∧ d)]))`", 
        Belt.Array.getExn(abstractions, 1), Parser.parse("(a ∧ (b ∧ [e/(c ∧ d)]))"))

    
    Assert.Ast.isEqualByName("`a and b and c and d` abstraction[2] should be `(a ∧ [f/(b ∧ (c ∧ d))])`", 
        Belt.Array.getExn(abstractions, 2), Parser.parse("(a ∧ [f/(b ∧ (c ∧ d))])"))

//
    Assert.Ast.isEqualByName("`a and b and c and d` abstraction[3] should be `[g/(a ∧ (b ∧ (c ∧ d)))]`", 
        Belt.Array.getExn(abstractions, 3), Parser.parse("[g/(a ∧ (b ∧ (c ∧ d)))]"))

    Assert.Ast.isNotEqualByName("`a and b and c and d` abstraction[3] should not be `[h/(a ∧ (b ∧ (c ∧ d)))]`", 
        Belt.Array.getExn(abstractions, 3), Parser.parse("[h/(a ∧ (b ∧ (c ∧ d)))]"))

    ignore()
})