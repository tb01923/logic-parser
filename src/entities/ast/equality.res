open Ast

let equals = (variableEquals) => {

    let opEqual = (opA, opB) => switch (opA, opB) {
    | (Conjunction, Conjunction) => true
    | (Disjunction, Disjunction) => true
    | (Conditional, Conditional) => true
    | (BiConditional, BiConditional) => true
    | (_, _) => false
    }

    let rec equals = (astA, astB) => switch (astA, astB) {
    | (Value(_, a), Value(_, b)) => a === b
    | (Negation(_, termA), Negation(_, termB)) => equals(termA, termB)
    | (Variable(_, a), Variable(_, b)) => variableEquals(a, b)
    | (BinaryOperation(_, opA, lhsA, rhsA), BinaryOperation(_, opB, lhsB, rhsB)) =>
        opEqual(opA, opB) && equals(lhsA, lhsB) && equals(rhsA, rhsB)
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
    | Some(stmt) =>  Debruinj.getDebruinjIndices(stmt)
    | None =>  Debruinj.getDebruinjIndices(stmtB)
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
