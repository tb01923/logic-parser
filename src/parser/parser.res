exception UnexpectedToken(Lexer.token)
exception NoTokens()

let peek = tokens => Belt.Array.get(tokens, 0)

let eat = (expectedType, tokens) => switch (expectedType, Js.Array.shift(tokens)) {
    | (Lexer.Or, Some(Lexer.Or)) => ()
    | (Lexer.And, Some(Lexer.And)) => ()
    | (Lexer.ThinRightArrow, Some(Lexer.ThinRightArrow)) => ()
    | (Lexer.FatDoubleArrow, Some(Lexer.FatDoubleArrow)) => ()
    | (Lexer.Not, Some(Lexer.Not)) => ()
    | (Lexer.Truth, Some(Lexer.Truth)) => ()
    | (Lexer.Falsity, Some(Lexer.Falsity)) => ()
    | (Lexer.Variable(a),Some(Lexer.Variable(b))) if a === b => ()
    | (_, Some(unexpectedToken)) => raise(UnexpectedToken(unexpectedToken))
    | (_, None) => raise(NoTokens)
}

let rec getTermValueOr = tokens => {
    switch peek( tokens ) {
        | Some(Lexer.Not) => {
            eat(Lexer.Not, tokens)
            let subExpression = getTermValueOr(tokens)
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
and getBiConditionalExpressions = tokens => {
    let higherPrecedenceExpression = getTermValueOr( tokens )
    switch peek( tokens ) {
        | Some(Lexer.FatDoubleArrow) => {
            eat(Lexer.FatDoubleArrow, tokens)
            Ast.makeBiConditional(higherPrecedenceExpression, getOrExpression(tokens))
        }
        | _ => higherPrecedenceExpression
    }
}
and getImpliesExpressions = tokens => {
    let higherPrecedenceExpression = getBiConditionalExpressions( tokens )
    switch peek( tokens ) {
        | Some(Lexer.ThinRightArrow) => {
            eat(Lexer.ThinRightArrow, tokens)
            Ast.makeImplies(higherPrecedenceExpression, getOrExpression(tokens))
        }
        | _ => higherPrecedenceExpression
    }
}
and getAndExpressions = tokens => {
    let higherPrecedenceExpression = getImpliesExpressions( tokens )
    switch peek( tokens ) {
        | Some(Lexer.And) => {
            eat(Lexer.And, tokens)
            Ast.makeAnd(higherPrecedenceExpression, getOrExpression(tokens))
        }
        | _ => higherPrecedenceExpression
    }
}
and getOrExpression = tokens => {
    let higherPrecedenceExpression = getAndExpressions( tokens )
    switch peek( tokens ) {
        | Some(Lexer.Or) => {
            eat(Lexer.Or, tokens)
            Ast.makeOr(higherPrecedenceExpression, getOrExpression(tokens))
        }
        | _ => higherPrecedenceExpression
    }
}
and getExpression = tokens => {
    // incase an operator is added lower than "or" in precedence
    getOrExpression( tokens )
}

let parse = statement => {
    let tokens = Lexer.getTokens(statement)
    getExpression(tokens)
}

let rec print = (node) =>
  switch (node) {
  | Ast.And(_, lhs, rhs) => print(lhs) ++ " ∧ " ++ print(rhs)
  | Ast.Or(_, lhs, rhs) => print(lhs) ++ " ∨ " ++ print(rhs)
  | Ast.Implies(_, lhs, rhs) => print(lhs) ++ " -> " ++ print(rhs)
  | Ast.BiConditional(_, lhs, rhs) => print(lhs) ++ " <=> " ++ print(rhs)
  | Ast.Not(_, term) => "¬" ++ print(term)
  | Ast.Variable(_, name) => name
  | Ast.Value(_, true) => "⊤"
  | Ast.Value(_, false) => "⊥"
  }

let a = parse("a and b or c and d <=> not a")
Js.Console.log(print(a))

//let tokens = [Lexer.LParen, Lexer.RParen]
//let x = eat(Lexer.LParen, tokens)
//let y = eat(Lexer.RParen, tokens)
//(x, y) -> ignore