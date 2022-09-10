/*
    Based on: https://fractaledmind.com/articles/ruby-logic-interpreter-2/
*/

const lexer = require('./lexer.js')
    , {Not, And, Or, Variable} = require("./ast.js")


const toAST = (tokens) => {

    const peek = (tokens) => tokens[0]
    const eat = p => (tokens) => {
      const t = tokens.shift()
      if( !p( t ) ) {
          throw "eat wrong tuype"
      }
    }
    const eatAnd = eat( lexer.isAnd )
    const eatOr = eat( lexer.isOr )
    const eatNot = eat( lexer.isNot )
    const eatVariable = eat( lexer.isVariable )
    const eatLParen = eat( lexer.isLParen )
    const eatRParen = eat( lexer.isRParen )


    // const getExpression = tokens => {
    //     const termOrExpression = getTermOrExpression( tokens )
    //     const currentToken = peek( tokens )
    //
    //     if ( lexer.isAnd( currentToken ) ) {
    //         eatAnd( tokens )
    //         return And( termOrExpression, getExpression( tokens ) )
    //     }
    //     else if ( lexer.isOr( currentToken ) ) {
    //         eatOr( tokens )
    //         return Or( termOrExpression, getExpression( tokens ) )
    //     }
    //
    //     return termOrExpression
    // }

    const getExpression = tokens => {
        const termOrExpression = getOrOperation( tokens )

        return termOrExpression
    }

    const getOrOperation = tokens => {
        const termOrExpression = getAndOperation( tokens )
        const currentToken = peek( tokens )

        if ( lexer.isOr( currentToken ) ) {
            eatOr( tokens )
            return Or( termOrExpression, getOrOperation( tokens ) )
        }

        return termOrExpression
    }

    const getAndOperation = tokens => {
        const termOrExpression = getTermOrExpression( tokens )
        const currentToken = peek( tokens )

        if ( lexer.isAnd( currentToken ) ) {
            eatAnd( tokens )
            return And( termOrExpression, getAndOperation( tokens ) )
        }


        return termOrExpression
    }

    const getTermOrExpression = tokens => {
        const currentToken = peek( tokens )

        if ( lexer.isNot( currentToken ) ) {
            eatNot( tokens )
            const term = getTerm( tokens )
            return Not( term );
        }
        else if ( lexer.isLParen( currentToken ) ) {
            eatLParen( tokens )
            const expression = getExpression( tokens )
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
            return Variable( currentToken.value() )
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