open Test

// todo: add tests for getExpressoin... 

test("/entities/ast/parser.res/parse", () => {
    let parsedAst = Parser.parse("a and b")

    let a = Ast.Variable("test a", "a")
    let b = Ast.Variable("test b", "b")
    let aAndB = Ast.BinaryOperation("test a and b", Conjunction, a, b)
    
    Assert.Ast.isEqualByName("Parse Binary Operation", parsedAst, aAndB)
})

test("/entities/ast/parser.res/eat", () => {
    Assert.Boolean.isTrue("Parser.eat(LParen, [LParen]) eats LParen", Parser.eat(Lexer.LParen, [Lexer.LParen]) == ())
    Assert.Parser.throwsNoTokens("Parser.eat(LParen, []) throws NoTokens", () => Parser.eat(Lexer.LParen, []))
    Assert.Parser.throwsUnexpectedToken("Parser.eat(LParen, [RParen]) throws UnexpecetdToken(RParen)", 
        () => Parser.eat(Lexer.LParen, [Lexer.RParen]), Lexer.RParen)

    let tokens = [Lexer.LParen, Lexer.Truth, Lexer.RParen]
    Assert.Boolean.isTrue("Parser.eat(LParen, " ++ Lexer.arrayToString(tokens) ++ ") eats LParen", 
        Parser.eat(Lexer.LParen, tokens) == ())
    Assert.Boolean.isTrue("Parser.eat(Truth, " ++ Lexer.arrayToString(tokens) ++ ") eats Truth", 
        Parser.eat(Lexer.Truth, tokens) == ())
    Assert.Boolean.isTrue("Parser.eat(RParen, " ++ Lexer.arrayToString(tokens) ++ ") eats RParen", 
        Parser.eat(Lexer.RParen, tokens) == ())
})