const { RecursiveVisitor } = require("./visitor")

const ast = new RecursiveVisitor({
    "And": (v, node) => `${node.constructor.name}(${node.lhs.accept(v)}, ${node.rhs.accept(v)})`,
    "Or": (v, node) => `${node.constructor.name}(${node.lhs.accept(v)}, ${node.rhs.accept(v)})`,
    "Not": (v, node) => `${node.constructor.name}(${node.term.accept(v)})`,
    "Variable": (v, node) => `${node.constructor.name}(${node.name})`,
})


const printImpl = (op, v, node) => `(${node.lhs.accept(v)} ${op} ${node.rhs.accept(v)})`
const implicitStatement = new RecursiveVisitor({
    "And": (v, node) => printImpl('and', v, node),
    "Or": (v, node) => printImpl('or', v, node),
    "Not": (v, node) => `not(${node.term.accept(v)})`,
    "Variable": (v, node) => `${node.name}`,
})

const implicitDebruinj = new RecursiveVisitor({
    "And": (v, node) => printImpl('and', v, node),
    "Or": (v, node) => printImpl('or', v, node),
    "Not": (v, node) => `not(${node.term.accept(v)})`,
    "Variable": (v, node) => `${node.debruinjIndex}`,
})

const printExpl = (op, v, node) => {
    if (node.explicitParens) {
        return `(${node.lhs.accept(v)} ${op} ${node.rhs.accept(v)})`
    }
    return `${node.lhs.accept(v)} ${op} ${node.rhs.accept(v)}`
}
const explicitStatement = new RecursiveVisitor({
    "And": (v, node) => printExpl('and', v, node),
    "Or": (v, node) => printExpl('or', v, node),
    "Not": (v, node) => `not(${node.term.accept(v)})`,
    "Variable": (v, node) => `${node.name}`,
})


module.exports = Object.freeze({
    ast,
    implicitStatement,
    explicitStatement,
    implicitDebruinj
})