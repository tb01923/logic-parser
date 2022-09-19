/*
    Based on: https://fractaledmind.com/articles/ruby-logic-interpreter-2/
*/

const lexer = require('./lexer.js')
    , {Not, And, Or, Variable, Implies} = require("../../model/ast/ast.js")


const toAST = (tokens) => {

    const peek = (tokens) => tokens[0]
    const eat = p => (tokens) => {
      const t = tokens.shift()
      if( !p( t ) ) {
          throw new Error("eat wrong type")
      }
    }
    const eatAnd = eat( lexer.isAnd )
    const eatOr = eat( lexer.isOr )
    const eatNot = eat( lexer.isNot )
    const eatVariable = eat( lexer.isVariable )
    const eatLParen = eat( lexer.isLParen )
    const eatRParen = eat( lexer.isRParen )
    const eatImplies = eat( lexer.isImplies )

    const getExpression = tokens => {
        const termOrExpression = getOrOperation( tokens )

        return termOrExpression
    }

    const getImplies = tokens => {
        const termOrExpression = getOrOperation( tokens )
        const currentToken = peek( tokens )

        if ( lexer.isImplies( currentToken ) ) {
            eatImplies( tokens )
            const lhs = termOrExpression
            const rhs = getImplies( tokens )
            return Implies.from( lhs, rhs )
        }

        return termOrExpression
    }

    const getOrOperation = tokens => {
        const termOrExpression = getAndOperation( tokens )
        const currentToken = peek( tokens )

        if ( lexer.isOr( currentToken ) ) {
            eatOr( tokens )
            const lhs = termOrExpression
            const rhs = getOrOperation( tokens )
            return Or.from( lhs, rhs )
        }

        return termOrExpression
    }

    const getAndOperation = tokens => {
        const termOrExpression = getTermOrExpression( tokens )
        const currentToken = peek( tokens )

        if ( lexer.isAnd( currentToken ) ) {
            eatAnd( tokens )
            const lhs = termOrExpression
            const rhs = getAndOperation( tokens )
            return And.from( lhs, rhs )
        }


        return termOrExpression
    }

    const getTermOrExpression = tokens => {
        const currentToken = peek( tokens )

        if ( lexer.isNot( currentToken ) ) {
            eatNot( tokens )
            const term = getTermOrExpression( tokens )
            return Not.from( term );
        }
        else if ( lexer.isLParen( currentToken ) ) {
            eatLParen( tokens )
            const expression = getExpression( tokens )
            expression.explicitParens = true
            eatRParen( tokens )
            return expression
        }
        else {
            const term = getTerm( tokens )
            return term
        }
    }
    
    const getTerm = tokens => {
        const currentToken = peek( tokens )

        if ( lexer.isVariable( currentToken ) ) {
            eatVariable( tokens )
            const name = currentToken.value
            return Variable.from( name )
        }
    }

    return getExpression( tokens )
}

const parse = line => {
    const tokens = lexer.lexer( line )
    const ast = toAST( tokens )
    return ast
}

module.exports = Object.freeze({
    parse
})