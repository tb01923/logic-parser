exception UnexpectedToken(Lexer.token)
exception NoTokens()

let peek = tokens => Belt.Array.get(tokens, 0)
let peekNext = tokens => Belt.Array.get(tokens, 1)

let eat = (expectedType, tokens) => switch (expectedType, Js.Array.shift(tokens)) {
    | (tokenA, Some(tokenB)) if Lexer.tokenEquals(tokenA, tokenB) => ()
    | (_, Some(unexpectedToken)) => raise(UnexpectedToken(unexpectedToken))
    | (_, None) => raise(NoTokens)
}

let operatorPrecendence = [
    (Lexer.Equal, Ast.makeEquivalence),
    (Lexer.FatDoubleArrow, Ast.makeBiConditional),
    (Lexer.ThinRightArrow, Ast.makeConditional),
    (Lexer.Or, Ast.makeDisjunction),
    (Lexer.And, Ast.makeConjunction)
]

let rec getParensNotOrAtom = tokens => switch peek( tokens ) {
    | Some(Lexer.Not) => {
        eat(Lexer.Not, tokens)
        let subExpression = getParensNotOrAtom(tokens)
        Ast.makeNegation(subExpression)
    }
    | Some(Lexer.LParen) => {
        eat(Lexer.LParen, tokens)
        let expression = getExpression(tokens)
        eat(Lexer.RParen, tokens)
        expression
    }
    | Some(Lexer.LBracket) => {
        eat(Lexer.LBracket, tokens)
        let abstraction = getAbstraction(tokens)
        eat(Lexer.RBracket, tokens)
        abstraction
    }
    | Some(Lexer.Variable(name)) => {
        eat(Lexer.Variable(name), tokens)
        Ast.makeVariable(name)
    }
    | Some(Lexer.Truth)  => {
        eat(Lexer.Truth, tokens)
        Ast.makeValue(true)
    }
    | Some(Lexer.Falsity) => {
        eat(Lexer.Falsity, tokens)
        Ast.makeValue(false)
    }
    | Some(token) => raise(UnexpectedToken(token))
    | None => raise(NoTokens)
}
and getBinaryOperation = (tokens, operations) => {

   // start by getting the next highest binary operation in precedence,
   //       if there isn't one get one of: grouping, not, or atom
    let higherPrecedentExpression = switch peekNext(operations) {
        | Some(_) => getBinaryOperation(tokens, Belt.Array.sliceToEnd(operations, 1))
        | None => getParensNotOrAtom(tokens)
    }

    //  if the next token matches this token for this binary operator,
    //      then construct cooresponding to this operation with the LHS of what was captured bvy looking 'higher'
    //       and rhs of recursing at this same level
    //      else just return the results of looking 'higher'
    switch (peek(tokens), peek(operations)) {
    | (Some(token), Some(expectedToken, constructor)) if Lexer.tokenEquals(token, expectedToken) => {
        eat(expectedToken, tokens)
        let lhs = higherPrecedentExpression
        let rhs = getBinaryOperation(tokens, operations)
        constructor(lhs, rhs)
    }
    | _ => higherPrecedentExpression
    }
}
and getAbstraction = tokens => {
    switch peek(tokens) {
    | Some(Lexer.Variable(name)) => {
        eat(Lexer.Variable(name), tokens)
        eat(Lexer.Slash, tokens)
        let expression = getExpression(tokens)
        Ast.makeAbstraction(name, expression)
    }
    | Some(token) => raise(UnexpectedToken(token))
    | None => raise(NoTokens)
    }
}
and getExpression = tokens => {
    getBinaryOperation(tokens, Belt.Array.copy(operatorPrecendence))
}


let parse = statement => {
    let tokens = Lexer.getTokens(statement)
    getExpression(tokens)
}


