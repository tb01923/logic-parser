const { parse: parse2_0 } = require("../../parser/2_0/parser")
const parse = parse2_0

// probably not the right dependency... maybe should duplicate?
const printVisitors = require("../../view/printVisitors")
const getDebRuinjString = (expression, knownDebruinj={}) => {
    const debruinjIndexes = expression.getDeBruinjIndex(knownDebruinj)
    const debruinj = expression.accept(printVisitors.implicitDebruinj(debruinjIndexes))
    return debruinj
}

// applyBiCOnditional is to remove recommendations that icrease complexity
//  we can remove this once we add the tree length to prune options
const law = (name, lawString, applyBiConditionally=true) => {
    const parts = lawString.split("<=>")
    const lhs = parse(parts[0])
    const rhs = parse(parts[1])
    ///////////////////////////////////////////////////////////////////////////////////////
    // in defining the laws the relative positions need to remain the same on both sides
    //      therefore set on the left, retrieve and pass into the right
    ///////////////////////////////////////////////////////////////////////////////////////
    // lhs.setDeBruinjIndex()
    // rhs.setDeBruinjIndex(lhs.knownDebruinjIndexes)
    return {name, lhs, rhs, lawString: lawString, applyBiConditionally}
}

const laws = [
    // this group doesn't make sense to reverse, makes problems more complicated
    law("Idempotence<or>", "p or p <=> p", false),
    law("Idempotence<and>", "p and p <=> p", false),
    law("Double Negation", "not not p <=> p", false),
    law("Absorbtion<or>", "p or (p and q) <=> p", false),
    law("Ansorbtion<and>", "p and (p or q) <=> p", false),
    // this group can be applied in both directions
    law("Commutative<and>", "p and q <=> q and p"),
    law("Commutative<or>", "p or q <=> q or p"),
    law("Associative<and>", "(p and q) and r <=> p and (q and r)"),
    law("Associative<or>", "(p or q) or r <=> p or (q or r)"),
    law("Distributative<or(and)>", "p or (q and r) <=> (p or q) and (p or r)"),
    law("Distributative<and(or)>", "p and (q or r) <=> (p and q) or (p and r)"),
    law("DeMorgan<not(or)>", "not(p or q) <=> not(p) and not(q)"),
    law("DeMorgan<not(and)>", "not(p and q) <=> not(p) or not(q)")
]

const identifyLaws = function (expression, matches = []) {
    // need relative rebase debruinj
    const expressionDebruinj = getDebRuinjString(expression)
    for (const law of laws) {
        const lhsDeBruinj = getDebRuinjString(law.lhs)
        const rhsDeBruinj = getDebRuinjString(law.rhs, law.lhs.getDeBruinjIndex())
        if (expressionDebruinj == lhsDeBruinj) {
            matches.push({
                name: law.name,
                expression,
                matchedLaw: law.lhs,
                translatedLaw: law.rhs,
                lawString: law.lawString
            })
        }
        if(law.applyBiConditionally && expressionDebruinj == rhsDeBruinj) {
            matches.push({
                name: law.name,
                expression,
                matchedLaw: law.rhs,
                translatedLaw: law.lhs,
                lawString:
                law.lawString
            })
        }
    }

    // find laews matching sub-expressions
    if (expression.lhs) {
        const lhsMatches = identifyLaws(expression.lhs)
        const rhsMatches = identifyLaws(expression.rhs)
        return matches.concat(lhsMatches).concat(rhsMatches)
    }
    else if (expression.term) {
        const termMatches = identifyLaws(expression.term)
        return matches.concat(termMatches)
    }

    return matches
}

const restateInNewAlphabet = function(matches) {

    const results = matches
        .map(law => {
            const oldAlpha = law.matchedLaw.getDeBruinjIndex({})
            const newAlpha = law.expression.getDeBruinjIndex({})
            const translatedLawInPreferredAlphabet = law.translatedLaw.fromNewAlphabet(oldAlpha, newAlpha)
            return {
                name: law.name,
                lawString: law.lawString,
                expression: law.expression,
                matchedLaw: law.matchedLaw.clone(),
                translatedLawInPreferredAlphabet
            }
        })
    return results
}

module.exports = Object.freeze({
    identifyLaws,
    restateInNewAlphabet
})