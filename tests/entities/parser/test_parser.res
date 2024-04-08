open Test

// test("Add", () => {
//   let add = (a,b) => a + b
//   Assert.Demo.intEqual(add(1, 1), 2)
//   Assert.Demo.intEqual(~message="1 + 2 === 3", add(1, 2), 3)
// })

test("/entities/ast/ast.res", () => {
  let t = Ast.Value("", true)
  let f = Ast.Value("", false)
  let a = Ast.Variable("" ,"a")
  let b = Ast.Variable("" ,"b")
  let notT = Ast.UnaryOperation("", Ast.Negation, Ast.Value("", true))
  let aAndB = Ast.BinaryOperation("", Ast.Conjunction, a, b)
  let aOrB = Ast.BinaryOperation("", Ast.Disjunction, a, b)
  let aImplB = Ast.BinaryOperation("", Ast.Conditional, a, b)
  let aBiImplB = Ast.BinaryOperation("", Ast.BiConditional, a, b)
  let aEquivA = Ast.BinaryOperation("", Ast.Equivalence, a, a)
  let abstractAandB = Ast.Abstraction("", "c", aAndB)

  Assert.Ast.isEqualByName("Construct Variable: `a`", a, Ast.makeVariable("a"))
  Assert.Ast.isEqualByName("Construct Value: `true`", t, Ast.makeValue(true))
  Assert.Ast.isEqualByName("Construct Negation: `not(true)`", notT, Ast.makeNegation(Ast.makeValue(true)))
  Assert.Ast.isEqualByName("Construct UnaryOperation `not(true)`", notT, Ast.makeUnaryOperation(Ast.Negation, Ast.makeValue(true)))
  Assert.Ast.isEqualByName("Construct BinaryOperation: `a and b`", aAndB, Ast.makeConjunction(a, b))
  Assert.Ast.isEqualByName("Construct BinaryOperation: `a or b`", aOrB, Ast.makeDisjunction(a, b))
  Assert.Ast.isEqualByName("Construct BinaryOperation: `a -> b`", aImplB, Ast.makeConditional(a, b))
  Assert.Ast.isEqualByName("Construct BinaryOperation: `a <=> b`", aBiImplB, Ast.makeBiConditional(a, b))
  Assert.Ast.isEqualByName("Construct BinaryOperation: `a == a`", aEquivA, Ast.makeEquivalence(a, a))
  Assert.Ast.isEqualByName("Construct Abstrtaction: `[c/(a and b)]`", abstractAandB, Ast.makeAbstraction("c", aAndB))

  Assert.Ast.isEqualByName("Get LHS", Ast.getLhs(aAndB), a)
  Assert.Ast.isEqualByName("Get RHS", Ast.getRhs(aAndB), b)
  Assert.Boolean.isTrue("Is Abstraction", Ast.hasAbstraction(abstractAandB))
    
})

test("/entities/ast/parser", () => {
    let parsedAst = Parser.parse("a and b")

    let a = Ast.Variable("test a", "a")
    let b = Ast.Variable("test b", "b")
    let aAndB = Ast.BinaryOperation("test a and b", Conjunction, a, b)
    
    Assert.Ast.isEqualByName("Parse Binary Operation", parsedAst, aAndB)

})