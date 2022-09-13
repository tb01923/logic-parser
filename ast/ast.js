const getNextDebuinjIndex = (knownVariables) =>
    Object
        .keys(knownVariables)
        .map(k => knownVariables[k])
        .reduce((max, current) => ((current > max) ? current : max), -1) + 1

class Propositional {
    constructor() { }

    setDeBruinjIndex = function(knownVariables={}) { }

    accept = function(visitor) {
        return visitor.visit(this)
    }
}

class UnaryOperation extends Propositional {
    constructor(term) {
        super()
        this.term = term
    }

    setDeBruinjIndex = function(knownVariables={}) { this.term.setDeBruinjIndex( knownVariables ) }

}

class BinaryOperation extends Propositional {
    constructor(lhs, rhs, explicitParens=false) {
        super()
        this.lhs = lhs
        this.rhs = rhs
        this.explicitParens = explicitParens
    }

    setDeBruinjIndex = function(knownVariables={}) {
        this.lhs.setDeBruinjIndex( knownVariables )
        this.rhs.setDeBruinjIndex( knownVariables )
    }

}

class And extends BinaryOperation {
    static from = (lhs, rhs, explicitParens=false) => new And(lhs, rhs, explicitParens)
}

class Or extends BinaryOperation {
    static from = (lhs, rhs, explicitParens=false) => new Or(lhs, rhs, explicitParens)
}

class Not extends UnaryOperation {
    static from = term => new Not(term)
}

class Variable extends Propositional {
    constructor(name) {
        super();
        this.name = name
        this.debruinjIndex = undefined
    }

    setDeBruinjIndex = function (knownVariables={}) {
        if( this.name in knownVariables ) {
            this.debruinjIndex = knownVariables[this.name]
        } else {
            const nextDebruinj = getNextDebuinjIndex(knownVariables)
            knownVariables[this.name] = nextDebruinj
            this.debruinjIndex = nextDebruinj
        }
    }

    static from = x => new Variable(x)
}

module.exports = Object.freeze({
    And,
    Or,
    Not,
    Variable
})
//C or A or (not(A) and B)