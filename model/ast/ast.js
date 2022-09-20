const { createHash, randomUUID } = require('crypto');

//from: https://www.geeksforgeeks.org/how-to-invert-key-value-in-javascript-object/#:~:text=invert()%20method%20of%20%E2%80%9Cunderscore,values%20and%20values%20as%20keys.
const invert = object => {
    var retobj = {};
    for(var key in object){
        retobj[object[key]] = key;
    }
    return retobj;
}

const getNextDebuinjIndex = (knownVariables) =>
    Object
        .keys(knownVariables)
        .map(k => knownVariables[k])
        .reduce((max, current) => ((current > max) ? current : max), -1) + 1

const sha256 = string => createHash('sha256').update(string).digest('hex');

/*
    Propositional =
        And: (Propositional, Propositional)
        | Or: (Propositional, Propositional)
        | Implies: (Propositional, Propositional)
        | Not: (Propositional)
        | Variable: (char)
 */
class Propositional {
    constructor() {
        this.id = randomUUID()
    }

    getDeBruinjIndex(knownVariables={}) { }

    accept(visitor) {
        return visitor.visit(this)
    }

    fromNewAlphabet(oldAlpha, newAlpha) {}
    clone() {}
    from() {}
    replace(id, expression) {}
}

class UnaryOperation extends Propositional {
    constructor(term, explicitParens = false) {
        super()
        this.term = term
        this.explicitParens = explicitParens
    }

    getDeBruinjIndex(knownVariables={}) {
        return this.term.getDeBruinjIndex( knownVariables )
    }

    replace(id, expression) {
        if (this.term.id === id) {
            this.term = expression.clone()
            return true
        } else {
            return this.term.replace(id, expression)
        }
    }

    fromNewAlphabet(oldAlpha, newAlpha) {
        return this.from(
            this.term.fromNewAlphabet(oldAlpha, newAlpha),
            this.explicitParens
        )
    }

    clone() {
        return this.from(
            this.term.clone(),
            this.explicitParens
        )
    }
}

class BinaryOperation extends Propositional {
    constructor(lhs, rhs, explicitParens=false) {
        super()
        this.lhs = lhs
        this.rhs = rhs
        this.explicitParens = explicitParens
    }

    getDeBruinjIndex(knownVariables={}) {
        knownVariables = this.lhs.getDeBruinjIndex( knownVariables )
        knownVariables = this.rhs.getDeBruinjIndex( knownVariables )
        return knownVariables
    }

    replace(id, expression) {
        if (this.lhs.id === id) {
            this.lhs = expression.clone()
            return true
        } else if (this.rhs.id === id) {
            this.rhs = expression.clone()
            return true
        } else {
            const replaced = this.lhs.replace(id, expression)
            if(!replaced) {
                return this.rhs.replace(id, expression)
            }
            return true
        }
    }

    fromNewAlphabet(oldAlpha, newAlpha) {
        return this.from(
            this.lhs.fromNewAlphabet(oldAlpha, newAlpha),
            this.rhs.fromNewAlphabet(oldAlpha, newAlpha),
            this.explicitParens
        )
    }

    clone() {
        return this.from(
            this.lhs.clone(),
            this.rhs.clone(),
            this.explicitParens
        )
    }

}

class And extends BinaryOperation {
    static from = (lhs, rhs, explicitParens=false) => new And(lhs, rhs, explicitParens)
    from = And.from
}

class Or extends BinaryOperation {
    static from = (lhs, rhs, explicitParens=false) => new Or(lhs, rhs, explicitParens)
    from = Or.from
}

class Implies extends BinaryOperation {
    static from = (lhs, rhs, explicitParens=false) => new Implies(lhs, rhs, explicitParens)
    from = Implies.from
}

class Not extends UnaryOperation {
    static from = (term, explicitParens=false) => new Not(term, explicitParens)
    from = Not.from
}

class Variable extends Propositional {
    constructor(name, debruinjIndex) {
        super();
        this.name = name
    }

    getDeBruinjIndex(knownVariables={}) {
        const not = b => !b
        if( not(this.name in knownVariables) ) {
            const nextDebruinj = getNextDebuinjIndex(knownVariables)
            knownVariables[this.name] = nextDebruinj
        }
        return knownVariables
    }

    static from = (x) => new Variable(x)
    from = Variable.from

    fromNewAlphabet(oldAlpha, newAlpha) {
        const debruinjIndex = oldAlpha[this.name]
        const newAlphaInverted = invert(newAlpha)

        const translated = newAlphaInverted[debruinjIndex]
        const newbie = Variable.from(translated)
        return newbie
    }

    replace(id, expression) {
        return false
    }

    clone() {
        return this.from(
            this.name
        )
    }
}

module.exports = Object.freeze({
    And,
    Or,
    Not,
    Variable,
    Implies
})