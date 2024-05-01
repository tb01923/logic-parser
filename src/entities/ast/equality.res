open Ast
open Debruinj

let binOpEquals = (opA, opB) => switch (opA, opB) {
| (Conjunction, Conjunction) => true
| (Disjunction, Disjunction) => true
| (Conditional, Conditional) => true
| (BiConditional, BiConditional) => true
| (Equivalence, Equivalence) => true
// better than double wild card, if new operator is added, this prevents the app from compiling
| (Conjunction, _) => false
| (Disjunction, _) => false
| (Conditional, _) => false
| (BiConditional, _) => false
| (Equivalence, _) => false
}

let unOpEquals = (opA, opB) => switch (opA, opB) {
| (Negation, Negation) => true
}


/**
    equalsWith: return a function that compares two propositions and returns true if they are equal
        though the @variableEquals coparrison of "named" (e.g., Variables, Abstractions) propositions  
 */
let equalsWith = (variableEquals) => {

    let rec equals = (astA, astB) => switch (astA, astB) {
    | (Value(_, a), Value(_, b)) => a === b
    | (UnaryOperation(_, opA, termA), UnaryOperation(_, opB, termB)) =>
        unOpEquals(opA, opB) && equals(termA, termB)
    | (Variable(_, a), Variable(_, b)) => variableEquals(a, b)
    | (BinaryOperation(_, opA, lhsA, rhsA), BinaryOperation(_, opB, lhsB, rhsB)) =>
        binOpEquals(opA, opB) && equals(lhsA, lhsB) && equals(rhsA, rhsB)
    // todo: maybe throw an exception IF symbols are equals, but the propositions are not???
    | (Abstraction(_, a, _), Abstraction(_, b, _)) => variableEquals(a, b)
    // todo: this seems like a hack, use variable to resolve debruinj index of abstraction
    | (_, Abstraction(_, symbol, _)) => equals(astA, makeVariable(symbol))
    | (Abstraction(_, symbol, _), _) => equals(makeVariable(symbol), astB)
    // if we are not handling it, it is definitely not a match
    | (_, _) => false
    }

    equals
}


/**
    byDebruinj: compare to propositions, compare two propositions @stmtA and @stmtB leveraging the debruinj indexing
        (the order in whcih they are introduced intot he proposition) of variable to assert whether two variables 
        are equal. @aCtxSrc and @bCtxSrc are hashtables that from variable name to the debruinj index are built internally 
        if not apssed in.  
 */
let byDebruinj = (~aCtxSrc=?, ~bCtxSrc=?, stmtA, stmtB) => {

    let ctxA = switch aCtxSrc {
    | Some(stmt) =>  Debruinj.getDebruinjIndices(stmt)
    | None =>  Debruinj.getDebruinjIndices(stmtA)
    }

    let ctxB = switch bCtxSrc {
    | Some(stmt) => getDebruinjIndices(stmt)
    | None => getDebruinjIndices(stmtB)
    }

    let variableIndexEquals = (a,b) => {
        let ai = Belt.HashMap.String.get(ctxA, a)
        let bi = Belt.HashMap.String.get(ctxB, b)
        ai === bi
    }
    let equalsWithVariableIndexEquals = equalsWith(variableIndexEquals)
    equalsWithVariableIndexEquals(stmtA, stmtB)
}

/**
    byName: compare to propositions, compare two propositions @stmtA and @stmtB leveraging the variable names to assert 
        whether two variables are equal.
 */
let byName = (stmtA, stmtB) => {
    let nameEquals = (a, b) => a === b
    let equalsWithNameEquals = equalsWith(nameEquals)
    equalsWithNameEquals(stmtA, stmtB)
}


/**
    byAbstractionResolution: compare to propositions, compare two propositions @stmtA and @stmtB this with @byName equality 
        but investigating the details of an abstraction
 */
let rec byAbstractionResolution = (astA, astB) =>
    switch (astA, astB) {
    | (Abstraction(_, _, opA), Abstraction(_, _, opB)) => byAbstractionResolution(opA, opB)
    // Binary Operation Equals with intentional fall through
    | (BinaryOperation(_, opA, lhsA, rhsA), Abstraction(_, _, BinaryOperation(_, opB, lhsB, rhsB)))
    | (Abstraction(_, _, BinaryOperation(_, opA, lhsA, rhsA)), BinaryOperation(_, opB, lhsB, rhsB)) =>
        binOpEquals(opA, opB) && byAbstractionResolution(lhsA, lhsB) && byAbstractionResolution(rhsA, rhsB)
    // UnaryOperation Equals with intentional fall through
    | (Abstraction(_, _, UnaryOperation(_, opA, termA)), UnaryOperation(_, opB, termB))
    | (UnaryOperation(_, opA, termA), Abstraction(_, _, UnaryOperation(_, opB, termB))) =>
        unOpEquals(opA, opB) && byAbstractionResolution(termA, termB)
    // defer to equality by variable name
    | (_, _) => byName(astA, astB)
    }

module PropositionCompare = Belt.Id.MakeComparableU({
  type t = proposition
  let cmp = (a, b) => switch byName(a, b) {
  | true => 0
  | false => 1
  }
})
