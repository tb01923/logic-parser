
const makeStack = function() {
    const data = []
    const push = (x) => data.push(x)
    const pop = () => data.pop(x)
    const isEmpty = () => data.length == 0
    const peek = () => !isEmpty() ? data[data.length-1] : undefined

    const vommit = () => data
    return Object.freeze({
        push, pop, isEmpty, peek, vommit
    })
}

module.exports = Object.freeze({
    makeStack
})