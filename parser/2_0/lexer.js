
class Token {
    constructor (type, value) {
        this.type = type
        this.value = value
    }

    static from = (type, value) => new Token(type, value)
}

const NOT = "NOT"
const AND = "AND"
const OR = "OR"
const VARIABLE = "VARIABLE"
const LPAREN= "("
const RPAREN = ")"

const is = type => t => t && (t.type== type)
const isVariable = is(VARIABLE)
const isAnd = is(AND)
const isOr  = is(OR)
const isNot = is(NOT)
const isLParen = is(LPAREN)
const isRParen = is(RPAREN)

const lexer = ( line ) => {

    const singleChar = /[a-zA-Z]/
    const whiteSpace = /\s+/

    const toToken = token => {
        if ( whiteSpace.test( token ) ) {
            return undefined
        }
        else if( token === "~" || token === "not") {
            return Token.from( NOT )
        }
        else if ( token === "and" ) {
            return Token.from( AND )
        }
        else if ( token === "or" ) {
            return Token.from( OR )
        }
        else if ( token === "(" ) {
            return Token.from( LPAREN )
        }
        else if ( token === ")" ) {
            return Token.from( RPAREN )
        }
        else if ( singleChar.test(token) ){
            return Token.from( VARIABLE, token )
        }
    }

    return line
        .split( /([\s()])/g )             // split on " " | "(" or ")"
        .filter( t => t !== " " )         // remove spaces
        .filter( t => t !== "" )          // remove empty strings
        .map( toToken )                   //=> Token
        .filter( t => t !== undefined )
}

module.exports = Object.freeze({
    lexer,
    isVariable,
    isAnd,
    isOr,
    isNot,
    isLParen,
    isRParen
})