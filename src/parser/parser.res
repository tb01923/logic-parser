exception UnexpectedToken(Lexer.token)
exception NoTokens()

let peek = tokens => Belt.Array.get(tokens, 0)
let next = tokens => Belt.Array.get(tokens, 1)

let eat = (expectedType, tokens) => switch (expectedType, Js.Array.shift(tokens)) {
    | (tokenA, Some(tokenB)) if Lexer.matches(tokenA, tokenB) => ()
    | (_, Some(unexpectedToken)) => raise(UnexpectedToken(unexpectedToken))
    | (_, None) => raise(NoTokens)
}

let operatorPrecendence = [
    (Lexer.Or, Ast.makeOr),
    (Lexer.And, Ast.makeAnd),
    (Lexer.ThinRightArrow, Ast.makeImplies),
    (Lexer.FatDoubleArrow, Ast.makeBiConditional)
]

let rec getParensNotOrAtom = tokens => {
    switch peek( tokens ) {
        | Some(Lexer.Not) => {
            eat(Lexer.Not, tokens)
            let subExpression = getParensNotOrAtom(tokens)
            Ast.makeNot(subExpression)
        }
        | Some(Lexer.LParen) => {
            eat(Lexer.LParen, tokens)
            let expression = getExpression(tokens)
            eat(Lexer.RParen, tokens)
            expression
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
}
//and getBiConditionalExpressions = tokens => {
//    let higherPrecedenceExpression = getParensNotOrAtom( tokens )
//    switch peek( tokens ) {
//        | Some(Lexer.FatDoubleArrow) => {
//            eat(Lexer.FatDoubleArrow, tokens)
//            Ast.makeBiConditional(higherPrecedenceExpression, getOrExpression(tokens))
//        }
//        | _ => higherPrecedenceExpression
//    }
//}
//and getImpliesExpressions = tokens => {
//    let higherPrecedenceExpression = getBiConditionalExpressions( tokens )
//    switch peek( tokens ) {
//        | Some(Lexer.ThinRightArrow) => {
//            eat(Lexer.ThinRightArrow, tokens)
//            Ast.makeImplies(higherPrecedenceExpression, getOrExpression(tokens))
//        }
//        | _ => higherPrecedenceExpression
//    }
//}
//and getAndExpressions = tokens => {
//    //let higherPrecedenceExpression = getImpliesExpressions( tokens )
//    let higherPrecedenceExpression = getParensNotOrAtom( tokens )
//    switch peek( tokens ) {
//        | Some(Lexer.And) => {
//            eat(Lexer.And, tokens)
//            let lhs = higherPrecedenceExpression
//            let rhs = getAndExpressions(tokens)
//
//            Ast.makeAnd(lhs, rhs)
//        }
//        | _ => higherPrecedenceExpression
//    }
//}
//and getOrExpression = tokens => {
//    let higherPrecedenceExpression = getAndExpressions( tokens )
//    switch peek( tokens ) {
//        | Some(Lexer.Or) => {
//            eat(Lexer.Or, tokens)
//            let lhs = higherPrecedenceExpression
//            let rhs = getOrExpression(tokens)
//            Ast.makeOr(lhs, rhs)
//        }
//        | _ => higherPrecedenceExpression
//    }
//}
and getBinaryOperation = (tokens, operations) => {

    // start by getting the next highest binary operation in precedence,
   //       if there isn't one get one of: grouping, not, or atom
    let higherPrecedentExpression = switch next(operations) {
        | Some(_) => getBinaryOperation(tokens, Belt.Array.sliceToEnd(operations, 1))
        | None => getParensNotOrAtom(tokens)
    }

    //  if the next token matches this token for this binary operator,
    //      then construct cooresponding to this operation with the LHS of what was captured bvy looking 'higher'
    //       and rhs of recursing at this same level
    //      else just return the results of looking 'higher'
    switch (peek(tokens), peek(operations)) {
        | (Some(token), Some(expectedToken, constructor)) if Lexer.matches(token, expectedToken) => {
            eat(expectedToken, tokens)
            let lhs = higherPrecedentExpression
            let rhs = getBinaryOperation(tokens, operations)
            constructor(lhs, rhs)
        }
        | _ => higherPrecedentExpression
    }

}
and getExpression = tokens => {
    // incase an operator is added lower than "or" in precedence
    //getOrExpression( tokens )
    let ops = Belt.Array.copy(operatorPrecendence)
    getBinaryOperation(tokens, ops)
}

let parse = statement => {
    let tokens = Lexer.getTokens(statement)
    getExpression(tokens)
}