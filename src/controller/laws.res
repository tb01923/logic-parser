/*TODO: non biconditional laws: modus ponens, modus tollens*/

exception NotBiConditional(string)

type law = (string, Ast.propositional, bool)
type matchedSide = LHS | RHS;
type transformation = {
    matchedLaw: law,
    matchedSide: matchedSide,
    statementMatched: Ast.propositional
}

let makeLaw = (~bidirectional=true, name, lawString) => {
    let ast = Parser.parse(lawString)
    switch ast {
    | Ast.BinaryOperation(_, op, _, _) => switch op {
        | BiConditional => (name, ast, bidirectional)
        | _ => raise(NotBiConditional(lawString))
    }
    | _ => raise(NotBiConditional(lawString))
    }
}

let makeTransformation = (law, side, statement) => {
    {matchedLaw: law, matchedSide: side, statementMatched: statement}
}

let laws = [
    // this group doesn't make sense to reverse, makes problems more complicated
    makeLaw("Idempotence<or>", "p or p <=> p", ~bidirectional=false),
    makeLaw("Idempotence<and>", "p and p <=> p", ~bidirectional=false),
    makeLaw("Double Negation", "not not p <=> p", ~bidirectional=false),
    makeLaw("Absorbtion<or>", "p or (p and q) <=> p", ~bidirectional=false),
    makeLaw("Absorbtion<and>", "p and (p or q) <=> p", ~bidirectional=false),
    // this group can be applied in both directions
    makeLaw("Commutative<and>", "p and q <=> q and p"),
    makeLaw("Commutative<or>", "p or q <=> q or p"),
    makeLaw("Associative<and>", "(p and q) and r <=> p and (q and r)"),
    makeLaw("Associative<or>", "(p or q) or r <=> p or (q or r)"),
    makeLaw("Distributive<or(and)>", "p or (q and r) <=> (p or q) and (p or r)"),
    makeLaw("Distributive<and(or)>", "p and (q or r) <=> (p and q) or (p and r)"),
    makeLaw("DeMorgan<not(or)>", "not(p or q) <=> not(p) and not(q)"),
    makeLaw("DeMorgan<not(and)>", "not(p and q) <=> not(p) or not(q)"),

    // should these be elsewhere
    makeLaw("truth-table-and", "T and T <=> T"),
    makeLaw("truth-table-and", "T and F <=> F"),
    makeLaw("truth-table-and", "F and T <=> F"),
    makeLaw("truth-table-and", "F and F <=> F"),
    makeLaw("truth-table-or", "T or T <=> T"),
    makeLaw("truth-table-or", "T or F <=> T"),
    makeLaw("truth-table-or", "F or T <=> T"),
    makeLaw("truth-table-or", "F or F <=> F"),
    makeLaw("truth-table-implies", "T -> T <=> T"),
    makeLaw("truth-table-implies", "T -> F <=> F"),
    makeLaw("truth-table-implies", "F -> T <=> T"),
    makeLaw("truth-table-implies", "F -> F <=> T"),
    makeLaw("truth-table-negate", "not F <=> T"),
    makeLaw("truth-table-negate", "not T <=> F"),
]

let attemptMatch = (statement, law) => {
    let (name, ast, bidirectional) = law

    // two partial applications
    //  1. ctx for lhs/rhs of law is derived from entire law (keep debruinj index consistent on both sides)
    //  2. statement applied to simplify reading the switch
    let equals = Equality.debruinj(~bCtxSrc=ast, statement)

    let biconditionalMatch = (lhs, rhs) => switch (bidirectional, equals(lhs), equals(rhs)) {
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
    | Ast.BinaryOperation(_, BiConditional, lhs, rhs) => biconditionalMatch(lhs, rhs)
    | Ast.BinaryOperation(_) => raise(NotBiConditional(name))
    | _ => raise(NotBiConditional(name))
    }
}

let rec identifyLaws = statement => {

    let theseMatches = laws
        -> Belt.Array.keepMap(attemptMatch(statement))
        -> Belt.Array.concatMany

    let subMatches = switch statement {
    | Negate(_, term) => identifyLaws(term)
    | BinaryOperation(_,_, lhs, rhs) => Belt.Array.concat(identifyLaws(lhs), identifyLaws(rhs))
    | _ => []
    }

    Belt.Array.concat(theseMatches, subMatches)
}