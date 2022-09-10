const getInstance = (self, constructor) =>
    (self instanceof constructor) ?
        self :
        Object.create(constructor.prototype) ;

const Token = function(type, value){
    const self = getInstance(this, Token)
    self.type = () => type
    self.value = () => value
    self.toString = function() {
        return `Token(${type}, ${value})`
    }
    return Object.freeze(self)
}

const NOT = "NOT"
const AND = "AND"
const OR = "OR"
const VARIABLE = "VARIABLE"
const LPAREN= "("
const RPAREN = ")"

const is = type => t => t && (t.type() == type)
const isVariable = is(VARIABLE)
const isAnd = is(AND)
const isOr  = is(OR)
const isNot = is(NOT)
const isLParen = is(LPAREN)
const isRParen = is(RPAREN)

const lexer = (line) => {

    const singleChar = /[a-zA-Z]/
    const whiteSpace = /\s+/

    const toToken = token => {
        if ( whiteSpace.test(token) ) {
            return undefined
        }
        else if( token == "~" ) {
            return Token( NOT )
        }
        else if ( token == "and" ) {
            return Token( AND )
        }
        else if ( token == "or" ) {
            return Token( OR )
        }
        else if ( token === "(" ) {
            return Token( LPAREN )
        }
        else if ( token === ")" ) {
            return Token( RPAREN )
        }
        else if ( singleChar.test(token) ){
            return Token( VARIABLE, token )
        }
    }

    return line
        .split(/([\s()])/g)             // split on " " | "(" or ")"
        .filter(t => t !== " ")         // remove spaces
        .filter(t => t !== "")          // remove empty strings
        .map(toToken)                   //=> Token
        .filter(t => t !== undefined)
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