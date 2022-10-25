open Ast
open Debruinj

let binOpEquals = (opA, opB) => switch (opA, opB) {
| (Conjunction, Conjunction) => true
| (Disjunction, Disjunction) => true
| (Conditional, Conditional) => true
| (BiConditional, BiConditional) => true
| (_, _) => false
}

let unOpEquals = (opA, opB) => switch (opA, opB) {
| (Negation, Negation) => true
}

let equals = (symbolEquals) => {

    let rec equals = (astA, astB) => switch (astA, astB) {
    | (Value(_, a), Value(_, b)) => a === b
    | (UnaryOperation(_, opA, termA), UnaryOperation(_, opB, termB)) =>
        unOpEquals(opA, opB) && equals(termA, termB)
    | (Variable(_, a), Variable(_, b)) => symbolEquals(a, b)
    | (BinaryOperation(_, opA, lhsA, rhsA), BinaryOperation(_, opB, lhsB, rhsB)) =>
        binOpEquals(opA, opB) && equals(lhsA, lhsB) && equals(rhsA, rhsB)
    // todo: maybe throw an exception IF symbols are equals, but the propositions are not???
    | (Abstraction(_, a, _), Abstraction(_, b, _)) => symbolEquals(a, b)
    // todo: this seems like a hack, use variable to resolve debruinj index of abstraction
    | (_, Abstraction(_, symbol, _)) => equals(astA, makeVariable(symbol))
    | (Abstraction(_, symbol, _), _) => equals(makeVariable(symbol), astB)
    // if we are not handling it, it is definitely not a match
    | (_, _) => false
    }

    equals
}

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
    equals(variableIndexEquals, stmtA, stmtB)
}

let byName = (stmtA, stmtB) => {
    let nameEquals = (a, b) => a === b
    equals(nameEquals, stmtA, stmtB)
}

let rec byAbstractionResolution = (astA, astB) =>
    switch (astA, astB) {
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

module PropositionCompare = Belt.Id.MakeComparable({
  type t = proposition
  let cmp = (a, b) => switch byName(a, b) {
  | true => 0
  | false => 1
  }
})
