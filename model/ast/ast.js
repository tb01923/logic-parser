const { createHash, randomUUID } = require('crypto');

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
        this.knownDebruinjIndexes = {}
    }

    setDeBruinjIndex(knownVariables={}) {
        this.knownDebruinjIndexes = knownVariables
    }

    accept(visitor) {
        return visitor.visit(this)
    }

    fromNewAlphabet(alphabet) {}
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

    setDeBruinjIndex(knownVariables={}) {
        this.term.setDeBruinjIndex( knownVariables )
        super.setDeBruinjIndex( knownVariables )
    }

    replace(id, expression) {
        if (this.term.id === id) {
            this.term = expression.clone()
            return true
        } else {
            return this.term.replace(id, expression)
        }
    }

    fromNewAlphabet(alphabet) {
        return this.from(
            this.term.fromNewAlphabet(alphabet),
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

    setDeBruinjIndex(knownVariables={}) {
        this.lhs.setDeBruinjIndex( knownVariables )
        this.rhs.setDeBruinjIndex( knownVariables )
        super.setDeBruinjIndex( knownVariables )
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

    fromNewAlphabet(alphabet) {
        return this.from(
            this.lhs.fromNewAlphabet(alphabet),
            this.rhs.fromNewAlphabet(alphabet),
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
        this.debruinjIndex = debruinjIndex
    }

    setDeBruinjIndex(knownVariables={}) {
        if( this.name in knownVariables ) {
            this.debruinjIndex = knownVariables[this.name]
        } else {
            const nextDebruinj = getNextDebuinjIndex(knownVariables)
            knownVariables[this.name] = nextDebruinj
            this.debruinjIndex = nextDebruinj
        }
        super.setDeBruinjIndex(knownVariables)
    }

    static from = (x, debruinjIndex) => new Variable(x, debruinjIndex)
    from = Variable.from

    fromNewAlphabet(alphabet) {
        const translated = alphabet[this.debruinjIndex]
        const newbie = Variable.from(translated, this.debruinjIndex)
        return newbie
    }

    replace(id, expression) {
        return false
    }

    clone() {
        return this.from(
            this.name,
            this.debruinjIndex
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