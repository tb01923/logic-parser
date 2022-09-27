open Laws
let makeTransformation = (law, side, statement) => {
    {matchedLaw: law, matchedSide: side, statementMatched: statement}
}

let attemptMatch = (statement, law) => {
    let (name, ast, bidirectional) = law

    // two partial applications
    //  1. ctx for lhs/rhs of law is derived from entire law (keep debruinj index consistent on both sides)
    //  2. statement applied to simplify reading the switch
    let equals = Equality.byDebruinj(~bCtxSrc=ast, statement)

    let equivalenceMatch = (lhs, rhs) => switch (bidirectional, equals(lhs), equals(rhs)) {
    // both sides of the law are matches, and law is applied bidirectionally
    | (true, true, true) => Some([
       makeTransformation(law, LHS, statement), makeTransformation(law, RHS, statement) ])
    // RHS of law matches, and law is applied bidirectionally
    | (true, false, true) => Some([ makeTransformation(law, RHS, statement) ])
    // always apply LHS if it matches
    | (_, true, _) => Some([ makeTransformation(law, LHS, statement) ])
    | (_) => None
    }

    // short-circuit if called with something otehr than BiConditional
    switch ast {
    | Ast.BinaryOperation(_, Equivalence, lhs, rhs) => equivalenceMatch(lhs, rhs)
    | Ast.BinaryOperation(_) => raise(ExpectingEquivalence(name))
    | _ => raise(ExpectingEquivalence(name))
    }
}

let rec identifyLaws = statement => {

    let theseMatches = laws
        -> Belt.Array.keepMap(attemptMatch(statement))
        -> Belt.Array.concatMany

    let subMatches = switch statement {
    | Negation(_, term) => identifyLaws(term)
    | BinaryOperation(_,_, lhs, rhs) => Belt.Array.concat(identifyLaws(lhs), identifyLaws(rhs))
    | _ => []
    }

    Belt.Array.concat(theseMatches, subMatches)
}