/*
TODO: three variable laws not working over statements with only two vars (e.g., "(a and b) and a")
TODO: non biconditional laws: modus ponens, modus tollens
*/

open Ast
exception ExpectingEquivalence(string)

type law = (string, Ast.proposition, bool)

let makeLaw = (~bidirectional=true, name, lawString) => {
    let ast = Parser.parse(lawString)
    switch ast {
    | BinaryOperation(_, op, _, _) => switch op {
        | Equivalence => (name, ast, bidirectional)
        | _ => raise(ExpectingEquivalence(lawString))
    }
    | _ => raise(ExpectingEquivalence(lawString))
    }
}

let laws = [
    // should these be elsewhere
    makeLaw("truth-table-and", "T and T = T", ~bidirectional=false),
    makeLaw("truth-table-and", "T and F = F", ~bidirectional=false),
    makeLaw("truth-table-and", "F and T = F", ~bidirectional=false),
    makeLaw("truth-table-and", "F and F = F", ~bidirectional=false),
    makeLaw("truth-table-or", "T or T = T", ~bidirectional=false),
    makeLaw("truth-table-or", "T or F = T", ~bidirectional=false),
    makeLaw("truth-table-or", "F or T = T", ~bidirectional=false),
    makeLaw("truth-table-or", "F or F = F", ~bidirectional=false),
    makeLaw("truth-table-implies", "T -> T = T", ~bidirectional=false),
    makeLaw("truth-table-implies", "T -> F = F", ~bidirectional=false),
    makeLaw("truth-table-implies", "F -> T = T", ~bidirectional=false),
    makeLaw("truth-table-implies", "F -> F = T", ~bidirectional=false),
    makeLaw("truth-table-biconditional", "T <=> T = T", ~bidirectional=false),
    makeLaw("truth-table-biconditional", "T <=> F = F", ~bidirectional=false),
    makeLaw("truth-table-biconditional", "F <=> T = F", ~bidirectional=false),
    makeLaw("truth-table-biconditional", "F <=> F = T", ~bidirectional=false),
    makeLaw("truth-table-negate", "not F = T", ~bidirectional=false),
    makeLaw("truth-table-negate", "not T = F", ~bidirectional=false),

    // this group doesn't make sense to reverse, makes problems more complicated
    makeLaw("Idempotence<or>", "p or p = p", ~bidirectional=false),
    makeLaw("Idempotence<and>", "p and p = p", ~bidirectional=false),
    makeLaw("Double Negation", "not not p = p", ~bidirectional=false),
    makeLaw("Absorbtion<or>", "p or (p and q) = p", ~bidirectional=false),
    makeLaw("Absorbtion<and>", "p and (p or q) = p", ~bidirectional=false),
    makeLaw("Tautology", "p or not(p) = T", ~bidirectional=false),
    makeLaw("Contradiction", "p and not(p) = F", ~bidirectional=false),
    // this group can be applied in both directions
    makeLaw("Commutative<and>", "p and q = q and p"),
    makeLaw("Commutative<or>", "p or q = q or p"),
    makeLaw("DeMorgan<not(or)>", "not(p or q) = not(p) and not(q)"),
    makeLaw("DeMorgan<not(and)>", "not(p and q) = not(p) or not(q)"),
    // restating each of these laws four times to account for
    //  1. three different variable being used OR
    //  2. two different variables being used OR
    //  3. one variable
    makeLaw("Associative<and>", "(p and q) and r = p and (q and r)"),
//    makeLaw("Associative<and2>", "(p and q) and p = p and (q and p)"),
//    makeLaw("Associative<and3>", "(p and q) and q = p and (q and q)"),
//    makeLaw("Associative<and4>", "(p and p) and p = p and (p and p)"),
    makeLaw("Associative<or>", "(p or q) or r = p or (q or r)"),
//    makeLaw("Associative<or2>", "(p or q) or p = p or (q or p)"),
//    makeLaw("Associative<or3>", "(p or q) or q = p or (q or q)"),
//    makeLaw("Associative<or4>", "(p or p) or p = p or (p or p)"),
    makeLaw("Distributive<or(and)>", "p or (q and r) = (p or q) and (p or r)"),
//    makeLaw("Distributive<or(and2)>", "p or (q and p) = (p or q) and (p or p)"),
//    makeLaw("Distributive<or(and3)>", "p or (q and q) = (p or q) and (p or q)"),
//    makeLaw("Distributive<or(and4)>", "p or (p and p) = (p or p) and (p or p)"),
    makeLaw("Distributive<and(or)>", "p and (q or r) = (p and q) or (p and r)"),
//    makeLaw("Distributive<and(or2)>", "p and (q or p) = (p and q) or (p and p)"),
//    makeLaw("Distributive<and(or3)>", "p and (q or q) = (p and q) or (p and q)"),
//    makeLaw("Distributive<and(or4)>", "p and (p or p) = (p and p) or (p and p)"),

]