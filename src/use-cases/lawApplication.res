open Ast
open Equality
open Laws

type matchedSide = LHS | RHS;
type transformation = {
    matchedLaw: law,
    matchedSide: matchedSide,
    matchedStatement: Ast.proposition
}

// todo: these belong in law
let getLawAst = (thisLaw: law) => {
    let (_, lawAst, _) = thisLaw
    lawAst
}
let getLawName = (thisLaw: law) => {
   let (name, _, _) = thisLaw
   name
}

let makeTransformation = (law, side, statement) => {
    {matchedLaw: law, matchedSide: side, matchedStatement: statement}
}

let equals = byDebruinj

let attemptMatch = (statement, law) => {
    let (name, ast, bidirectional) = law

    // two partial applications
    //  1. ctx for lhs/rhs of law is derived from entire law (keep debruinj index consistent on both sides)
    //  2. statement applied to simplify reading the switch
    let equalsStatement = statementB => equals(~bCtxSrc=ast, statement, statementB)

    let equivalenceMatch = (lhs, rhs) => switch (bidirectional, equalsStatement(lhs), equalsStatement(rhs)) {
    // both sides of the law are matches, and law is applied bidirectionally
    | (true, true, true) => Some([
       makeTransformation(law, LHS, statement), makeTransformation(law, RHS, statement) ])
    // RHS of law matches, and law is applied bidirectionally
    | (true, false, true) => Some([ makeTransformation(law, RHS, statement) ])
    // always apply LHS if it matches
    | (_, true, _) => Some([ makeTransformation(law, LHS, statement) ])
    | (_) => None
    }

    // short-circuit if called with something otehr than Equivalence
    //  todo: consider how to handle non-equivalence laws
    switch ast {
    | BinaryOperation(_, Equivalence, lhs, rhs) => equivalenceMatch(lhs, rhs)
    | BinaryOperation(_) => raise(ExpectingEquivalence(name))
    | _ => raise(ExpectingEquivalence(name))
    }
}

let rec identifyLaws = statement => {

    let attemptToMatchLaw = law => attemptMatch(statement, law)
    let theseMatches = laws
        -> Belt.Array.keepMap(attemptToMatchLaw)
        -> Belt.Array.concatMany

    let subMatches = switch statement {
    | UnaryOperation(_, _, term) => identifyLaws(term)
    | BinaryOperation(_,_, lhs, rhs) => Belt.Array.concat(identifyLaws(lhs), identifyLaws(rhs))
    | _ => []
    }

    Belt.Array.concat(theseMatches, subMatches)
}

