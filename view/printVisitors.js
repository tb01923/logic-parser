const { RecursiveVisitor } = require("./visitor")

const printBinary = (v, node) => `${node.constructor.name}(${node.lhs.accept(v)}, ${node.rhs.accept(v)})`
const ast = new RecursiveVisitor({
    "And": printBinary,
    "Or": printBinary,
    "Implies": printBinary,
    "Not": (v, node) => `${node.constructor.name}(${node.term.accept(v)})`,
    "Variable": (v, node) => `${node.constructor.name}(${node.name})`,
})

const printImpl = (op, v, node) => `(${node.lhs.accept(v)} ${op} ${node.rhs.accept(v)})`
const implicitStatement = new RecursiveVisitor({
    "And": (v, node) => printImpl('and', v, node),
    "Or": (v, node) => printImpl('or', v, node),
    "Implies": (v, node) => printImpl('->', v, node),
    "Not": (v, node) => `not(${node.term.accept(v)})`,
    "Variable": (v, node) => `${node.name}`,
})

const implicitDebruinj = (debruinjIndexes) => new RecursiveVisitor({
    "And": (v, node) => printImpl('and', v, node),
    "Or": (v, node) => printImpl('or', v, node),
    "Implies": (v, node) => printImpl('->', v, node),
    "Not": (v, node) => `not(${node.term.accept(v)})`,
    "Variable": (v, node) => `${debruinjIndexes[node.name]}`,
})

const printBinaryExpl = (op, v, node) => {
    if (node.explicitParens) {
        return `(${node.lhs.accept(v)} ${op} ${node.rhs.accept(v)})`
    }
    return `${node.lhs.accept(v)} ${op} ${node.rhs.accept(v)}`
}

const printUnaryExpl = (op, v, node) => {
    if (node.explicitParens) {
        return `${op}(${node.term.accept(v)})`
    }
    return `${op} ${node.term.accept(v)}`
}
const explicitStatement = new RecursiveVisitor({
    "And": (v, node) => printBinaryExpl('and', v, node),
    "Or": (v, node) => printBinaryExpl('or', v, node),
    "Implies": (v, node) => printBinaryExpl('->', v, node),
    "Not": (v, node) => printUnaryExpl("not", v, node),
    "Variable": (v, node) => `${node.name}`,
})


module.exports = Object.freeze({
    ast,
    implicitStatement,
    explicitStatement,
    implicitDebruinj
})