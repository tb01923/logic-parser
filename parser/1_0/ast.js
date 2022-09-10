
const flatten = (prop) => (obj) => {
    if(Object.keys(obj).length == 1) {
        return (obj[prop]) ? obj[prop] : obj
    }
    else {
        return obj
    }
}
const lastNode = node => {
    if (!node.rhs) {
        return node
    } else if (node.rhs && node.rhs.variable) {
        return node
    }
    return lastNode(node.rhs)
}
const joiner = (agg, next) => {

    const last = lastNode(agg)
    if (!agg.lhs) {
        // if the agg is missing the LHS then the agg hasn't been initialized yet
        agg.lhs = next
    } else if (last.rhs) {
        // if there is an rhs we need to push it down a layer
        const parentRhs = last.rhs
        last.rhs = {}
        last.rhs.lhs = parentRhs
        last.rhs.op = next.op
        last.rhs.rhs = next.rhs

    } else {
        // otherwise lhs is built looking for operator and rhs

        agg.op = next.op
        agg.rhs = next.rhs
    }

    return agg

}
const join = (x, xs) => {
    return [x].concat(xs).reduce(joiner, {})
}

module.exports = Object.freeze({
    join,
    flatten
})