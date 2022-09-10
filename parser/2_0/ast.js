const getInstance = (self, constructor) =>
    (self instanceof constructor) ?
        self :
        Object.create(constructor.prototype) ;

const AND = "AND"
const OR = "OR"
const NOT = "NOT"
const VARIABLE = "VARIABLE"

const And = function(lhs, rhs) {
    const self = getInstance(this, And)
    self.type = () => AND
    self.lhs = () => lhs
    self.rhs = () => rhs
    self.toString = function() {
        return `And(${lhs}, ${rhs})`
    }
    return Object.freeze(self)
}

const Or = function(lhs, rhs) {
    const self = getInstance(this, Or)
    self.type = () => OR
    self.lhs = () => lhs
    self.rhs = () => rhs
    self.toString = function() {
        return `Or(${lhs}, ${rhs})`
    }
    return Object.freeze(self)
}

const Not = function(term) {
    const self = getInstance(this, Not)
    self.type = () => NOT
    self.term = () => term
    self.toString = function() {
        return `Not(${term})`
    }
    return Object.freeze(self)
}

const Variable = function(name) {
    const self = getInstance(this, Variable)
    self.type = () => VARIABLE
    self.name = () => name
    self.toString = function() {
        return `Variable(${name})`
    }
    return Object.freeze(self)
}

module.exports = Object.freeze({
    And,
    Or,
    Not,
    Variable
})